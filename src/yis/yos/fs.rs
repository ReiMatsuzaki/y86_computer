// implimentation of vsfs written in OSTEP chapter 40 File System Implementation

use crate::yis::ram::{read_as_words, write_words};

const BLOCK_BYTES: usize = 4 * 1024;
const DATA_REGION_OFFSET: usize = 8 * BLOCK_BYTES;
const INODE_REGION_OFFSET: usize = 3 * BLOCK_BYTES;
const IBMAP_OFFSET: usize = 1 * BLOCK_BYTES;
const DBMAP_OFFSET: usize = 2 * BLOCK_BYTES;

pub struct FileSystem {
    // FIXME: communicate disk via Memory mapped I/O
    disk: Disk,
    // block: [u8; 4*1024],
}

#[derive(Debug)]
pub enum FileSystemError {
    FileNotFound,
    OutOfSector,
    ExceedingBlockLimit,
    InvalidINodeMode,
    UnAllocated,
}

type Res<T> = Result<T, FileSystemError>;

impl FileSystem {
    pub fn new() -> FileSystem {
        // FIXME: allocate first data region as root directory
        FileSystem {
            disk: Disk::new()
        }
    }

    pub fn read_inode(&self, inum: usize) -> Result<INode, FileSystemError> {
        if !(self.read_bmap(inum, IBMAP_OFFSET)?) {
            return Err(FileSystemError::UnAllocated);
        }

        let mut buf = [0; SECTOR_BYTES];
        let sector = inum + INODE_REGION_OFFSET / SECTOR_BYTES;
        self.disk.read_sector(sector, &mut buf)
           .ok_or(FileSystemError::OutOfSector)?;
        let size = read_as_words(&buf, 0) as usize;
        let num_block = buf[8];
        let mut blocks = [0; 15];
        for i in 0..num_block {
            let i = i as usize;
            blocks[i] = read_as_words(&buf, 9 + i * 8);
        }
        let mode = buf[9 + 15 * 8];
        let mode = match mode {
            0 => INodeMode::RegularFile,
            1 => INodeMode::Directory,
            _ => return Err(FileSystemError::InvalidINodeMode),
        };
        Ok(INode {inum, size, num_block, blocks, mode})
    }

    fn write_inode(&mut self, inode: &INode) -> Result<(), FileSystemError> {
        let mut buf = [0u8; SECTOR_BYTES];

        write_words(&mut buf, 0, inode.size as u64);
        buf[8] = inode.num_block;
        for i in 0..inode.num_block {
            let i = i as usize;
            write_words(&mut buf, 9 + i * 8, inode.blocks[i]);
        }
        buf[9 + 15 * 8] = match inode.mode {
            INodeMode::RegularFile => 0,
            INodeMode::Directory => 1,
        };

        let sector = inode.inum + INODE_REGION_OFFSET / SECTOR_BYTES;
        self.disk.write_sector(sector, &mut buf)
            .ok_or(FileSystemError::OutOfSector)?;

        self.write_bmap(inode.inum, 1 * BLOCK_BYTES, true)?;

        Ok(())
    }

    fn read_bmap(&self, inum: usize, offset: usize) -> Result<bool, FileSystemError> {
        let x = inum / 8;
        let y = inum % 8;
        let mut buf = [0; SECTOR_BYTES];
        let sector = offset / SECTOR_BYTES;
        self.disk.read_sector(sector, &mut buf)
            .ok_or(FileSystemError::OutOfSector)?;

        let mask = 1 << y;
        let x = buf[x] & mask;
        Ok(x > 0)
    }

    fn write_bmap(&mut self, inum: usize, offset: usize, flag: bool) -> Result<(), FileSystemError> {
        let x = inum / 8;
        let y = inum % 8;
        let mut buf = [0; SECTOR_BYTES];
        let sector = offset / SECTOR_BYTES;
        self.disk.read_sector(sector, &mut buf)
            .ok_or(FileSystemError::OutOfSector)?;

        let v = buf[x];
        let v = if flag {
            v | (1 << y)
        } else {
            v & !(1 << y)
        };
        buf[x] = v;
        self.disk.write_sector(sector, &mut buf)
            .ok_or(FileSystemError::OutOfSector)?;
        Ok(())
    }

    fn alloc_bmap(&mut self, offset: usize) -> Res<usize> {
        let mut buf = [0; SECTOR_BYTES];
        let sector = offset / SECTOR_BYTES;
        self.disk.read_sector(sector, &mut buf)
            .ok_or(FileSystemError::OutOfSector)?;
        let mut inum = 1000;
        for i in 0..100 {
            let b = (buf[i / 8] >> (i % 8)) & 0x1;
            if b == 0 {
                inum = i;
                break;
            }
        }
        if inum == 1000 {
            return Err(FileSystemError::ExceedingBlockLimit);
        }
        self.write_bmap(inum, offset, true)?;
        Ok(inum)
    }

    fn read_dir(&self, inode: &INode) -> Res<Directory> {
        if inode.mode != INodeMode::Directory {
            return Err(FileSystemError::InvalidINodeMode);
        }

        let mut buf = [0u8; SECTOR_BYTES];
        let addr = DATA_REGION_OFFSET + (inode.blocks[0] as usize) * BLOCK_BYTES;
        let sector = addr / SECTOR_BYTES ;
        self.disk.read_sector(sector, &mut buf);

        let mut i = 0;
        let mut dirents = vec![];
        while i < inode.size {
            let inum = read_as_words(&buf, i) as usize;
            i += 8;
            let name_len = buf[i] as usize;
            i += 1;
            let mut name = [0u8; 16];
            for j in 0..16 {
                name[j] = buf[i + j]
            }
            i += 16;
            let name = &name[..name_len];
            let name = String::from_utf8(name.to_vec())
                .map_err(|_| FileSystemError::FileNotFound)?; // FIXME: error handling
            dirents.push(Dirent {inum, name});
        }
        if dirents.len() == 0 {
            return Err(FileSystemError::FileNotFound);
        }
        Ok(Directory {dirents})
    }

    fn write_dir(&mut self, inode: &INode, dir: &Directory) -> Res<()> {
        if inode.mode != INodeMode::Directory {
            return Err(FileSystemError::InvalidINodeMode);
        }

        let mut buf = [0u8; SECTOR_BYTES];

        let mut i = 0;
        for dirent in &dir.dirents {
            write_words(&mut buf, i, dirent.inum as u64);
            i += 8;
            buf[i] = dirent.name.len() as u8;
            i += 1;
            for j in 0..(dirent.name.len()) {
                buf[i + j] = dirent.name.as_bytes()[j];
            }
            i += 16;
        }

        let addr = DATA_REGION_OFFSET + (inode.blocks[0] as usize) * BLOCK_BYTES;
        let sector = addr / SECTOR_BYTES ;
        self.disk.write_sector(sector, &buf);

        Ok(())
    }

    pub fn create_dir(&mut self, parent: Option<(&INode, String)>) -> Res<INode> {
        let inum = self.alloc_bmap(IBMAP_OFFSET)?;
        let dnum = self.alloc_bmap(DBMAP_OFFSET)?;

        let dirent = Dirent {inum, name: String::from(".")};
        let dirents = match parent {
            Some((p, _)) => vec![
                dirent,
                Dirent {inum: p.inum, name: String::from("..")},
            ],
            None => vec![dirent]
        };
        let dirents_len = dirents.len();
        let dir = Directory { dirents };

        let mut blocks: [u64; 15] = [0; 15];
        blocks[0] = dnum as u64;
        let inode = INode {
            inum,
            size: dirents_len * 25,
            num_block: 1,
            blocks,
            mode: INodeMode::Directory,
        };
        self.write_inode(&inode)?;
        self.write_dir(&inode, &dir)?;

        if let Some((p, name)) = parent {
            let mut inode = self.read_inode(p.inum)?;
            let mut d = self.read_dir(&inode)?;
            d.dirents.push(Dirent {
                inum,
                name,
            });
            inode.size += 25;
            self.write_inode(&inode)?;
            self.write_dir(&inode, &d)?;
        }
        Ok(inode)
    }

    pub fn read(&self, file: &mut File, buf: &mut [u8], len: usize) -> Res<usize> {
        if file.inode.mode != INodeMode::RegularFile {
            return Err(FileSystemError::InvalidINodeMode);
        }
        assert!(file.inode.num_block == 1);
        let dnum = file.inode.blocks[0] as usize;
        let sector = (DATA_REGION_OFFSET + dnum * BLOCK_BYTES) / SECTOR_BYTES;

        let mut sector_buf = [0u8; SECTOR_BYTES];
        self.disk.read_sector(sector, &mut sector_buf);
        for i in file.off..file.off+len {
            buf[i] = sector_buf[i];
        }

        Ok(len)
    }

    pub fn write(&mut self, file: &mut File, buf: &[u8], len: usize) -> Res<usize> {
        if file.inode.mode != INodeMode::RegularFile {
            return Err(FileSystemError::InvalidINodeMode);
        }
        assert!(file.inode.num_block == 1);
        let dnum = file.inode.blocks[0] as usize;
        let sector = (DATA_REGION_OFFSET + dnum * BLOCK_BYTES) / SECTOR_BYTES;

        let mut sector_buf = [0u8; SECTOR_BYTES];

        for i in file.off..file.off+len {
            sector_buf[i] = buf[i];
        }
        file.off = file.off + len;
        self.disk.write_sector(sector, &mut sector_buf);

        Ok(len)
    }

    pub fn create_file(&mut self, parent: &INode, name: &str) -> Res<File> {
        let inum = self.alloc_bmap(IBMAP_OFFSET)?;
        let dnum = self.alloc_bmap(DBMAP_OFFSET)?;

        let mut inode_dir = self.read_inode(parent.inum)?;
        let mut dir = self.read_dir(&inode_dir)?;
        dir.dirents.push(Dirent {
            inum,
            name: name.to_string(),
        });
        inode_dir.size += 25;
        self.write_inode(&inode_dir)?;
        self.write_dir(&inode_dir, &dir)?;

        let mut blocks: [u64; 15] = [0; 15];
        blocks[0] = dnum as u64;
        let inode = INode {
            inum,
            size: 0,
            num_block: 1,
            blocks,
            mode: INodeMode::RegularFile,
        };

        self.write_inode(&inode)?;

        let file = File::new(inode);
        Ok(file)
    }

    fn find_for_dir(&self, dir: &Directory, name: &str) -> Result<INode, FileSystemError> {
        if let Some(i) = name.find("/") {
            // dir
            let dir_name = &name[..i];
            let resid_name = &name[(i+1)..];
            let dirent = dir.dirents.iter().find(|d| d.name.eq(dir_name))
                .ok_or(FileSystemError::FileNotFound)?;
            let inode = self.read_inode(dirent.inum as usize)?;
            let dir = &self.read_dir(&inode)?;
            self.find_for_dir(dir, resid_name)
        } else {
            // file
            let dirent = dir.dirents.iter().find(|d| d.name.eq(name))
                .ok_or(FileSystemError::FileNotFound)?;
            let inode = self.read_inode(dirent.inum as usize)?;
            Ok(inode)
        }
    }

    pub fn find(&self, name: &str) -> Result<INode, FileSystemError> {
        // fetch root inode
        let root_inode = self.read_inode(0)?;
        let root_dir = self.read_dir(&root_inode)?;
        if let Some("/") = name.get(0..1) {
            let name = &name[1..];
            self.find_for_dir(&root_dir, name)
        } else {
            Err(FileSystemError::FileNotFound)
        }
    }

}

pub struct File {
    inode: INode,
    off: usize,
}

#[cfg(test)]
impl File {
    pub fn reset(&mut self) {
        self.off = 0;
    }
}

#[derive(Debug, PartialEq)]
pub struct Directory {
    dirents: Vec<Dirent>
}

#[derive(Debug, PartialEq)]
pub struct Dirent {
    inum: usize,
    name: String
    // name_len: u8,
    // name: [u8; 16],
}

impl File {
    pub fn new(inode: INode) -> File {
        File {
            inode,
            off: 0,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct INode {
    inum: usize,
    size: usize,
    num_block: u8,
    blocks: [u64; 15],
    mode: INodeMode
}

#[derive(Debug, PartialEq)]
enum INodeMode {
    RegularFile,
    Directory,
}

// ==== below code is hardware =====
// 1 Block = 4K byte = 8 Sectors
// 1 Sector = 512 byte
// Disk is virtual device which transfer sector size data
// FIXME: define Disk in yis/disk.rs

const SECTOR_BYTES: usize = 512;
const DISK_BYTES: usize = 64 * 4 * 1024;

pub struct Disk {
    bytes: [u8; DISK_BYTES] // 64 blocks
}

impl Disk {
    pub fn new() -> Disk {
        Disk {
            bytes: [0; DISK_BYTES]
        }
    }

    pub fn read_sector(&self, sector: usize, buf: &mut [u8]) -> Option<()> {
        if sector >= DISK_BYTES / SECTOR_BYTES {
            return None
        }
        buf.copy_from_slice(&self.bytes[sector*SECTOR_BYTES..(sector+1)*SECTOR_BYTES]);
        Some(())
    }

    pub fn write_sector(&mut self, sector: usize, buf: &[u8]) -> Option<()> {
        if sector >= DISK_BYTES / SECTOR_BYTES {
            return None
        }
        self.bytes[sector*SECTOR_BYTES..(sector+1)*SECTOR_BYTES].copy_from_slice(&buf);
        Some(())
    }
}

// #[cfg(test)]
// impl Disk {
//     fn print(&self, s: usize, len: usize) {
//         for i in (s/16)..((s+len)/16) {
//             let j = 16 * i;
//             println!("{:?}", &self.bytes[j..j+16]);
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bmap() {
        let offs = vec![IBMAP_OFFSET, DBMAP_OFFSET];
        let mut fs = FileSystem::new();
        let flags = vec![
            true, false, true, false, true, false, true, false,
            false, true, false, true, false, true, false, true, 
        ];
        for off in  offs {
            for inum in 0..flags.len() {
                let flag = flags[inum];
                fs.write_bmap(inum, off, flag).unwrap();
                let res = fs.read_bmap(inum, off).unwrap();
                assert_eq!(res, flag);
            }
        }
    }

    #[test]
    fn test_inode() {
        fn f(inum: usize) -> INode {
            let mut blocks: [u64; 15] = [0; 15];
            blocks[0] = DATA_REGION_OFFSET as u64;
            let inode = INode {
                inum,
                size: inum*5,
                num_block: 1,
                blocks,
                mode: INodeMode::Directory,
            };
            inode
        }
        let mut fs = FileSystem::new();

        for inum in 1..5 {
            let inode = f(inum);
            fs.write_inode(&inode).unwrap();
        }

        for inum in 1..5 {
            let x = f(inum);
            let y = fs.read_inode(inum).unwrap();
            assert_eq!(x, y);
        }
    }

    #[test]
    fn test_dir() {
        let mut fs = FileSystem::new();
        let inode = fs.create_dir(None).unwrap();
        let dnum = inode.blocks[0] as usize; 

        assert_eq!(inode.inum, 0);
        assert_eq!(dnum, 0);
        assert!(fs.read_bmap(inode.inum, IBMAP_OFFSET).unwrap());
        assert!(fs.read_bmap(dnum, DBMAP_OFFSET).unwrap());

        let inode_2 = fs.read_inode(0).unwrap();
        assert_eq!(inode, inode_2);

        let dir = fs.read_dir(&inode).unwrap();
        // println!("{:?}", dir.dirents);
        assert_eq!(dir.dirents.len(), 1);
        assert_eq!(dir.dirents[0].name, ".");

        let inode_ch1 = fs.create_dir(
            Some((&inode, "dira".to_string()))).unwrap();
        
        let inode_ch2 = fs.create_dir(
            Some((&inode, "dirb".to_string()))).unwrap();
        // for b in 0..1 {
        //     for i in 0..4 {
        //         let j = DATA_REGION_OFFSET+BLOCK_BYTES*b + 25*i;
        //         println!("{:?}", &fs.disk.bytes[j..j+25]);
        //     }
        //     println!("");
        // }

        let dir_ch1 = fs.read_dir(&inode_ch1).unwrap();
        let dir_ch2 = fs.read_dir(&inode_ch2).unwrap();
        assert_eq!(inode_ch2.inum, 2);
        assert_eq!(inode_ch2.blocks[0] as usize, 2);
        assert_eq!(dir_ch1.dirents.len(), 2);
        assert_eq!(dir_ch2.dirents[0].name, ".");
        assert_eq!(dir_ch2.dirents[1].name, "..");

        let inode = fs.read_inode(0).unwrap();
        let dir = fs.read_dir(&inode).unwrap();
        assert_eq!(dir.dirents.len(), 3);
        
    }

    #[test]
    fn test_file() {
        let mut fs = FileSystem::new();
        let inode_root = fs.create_dir(None).unwrap();
        let mut file = fs.create_file(&inode_root, "file1").unwrap();

        let msg = "hello world";
        let len = msg.len();
        let buf = msg.as_bytes();
        fs.write(&mut file, buf, len).unwrap();
        file.reset();

        let mut buf = [0; 1024];
        fs.read(&mut file, &mut buf, len).unwrap();
        let buf = String::from_utf8(buf[..len].to_vec()).unwrap();
        assert_eq!(buf, "hello world");
    }

    #[test]
    fn test_find_file() {
        let mut fs = FileSystem::new();
        let inode_root = fs.create_dir(None).unwrap();
        
        let inode_dira = fs.create_dir(Some((&inode_root, "dira".to_string()))).unwrap();
        let inode_dirb = fs.create_dir(Some((&inode_dira, "dirb".to_string()))).unwrap();
        let filec = fs.create_file(&inode_dirb, "filec").unwrap();

        // println!("");
        // fs.disk.print(INODE_REGION_OFFSET + 0 * SECTOR_BYTES, 25);
        // fs.disk.print(INODE_REGION_OFFSET + 1 * SECTOR_BYTES, 25);
        // fs.disk.print(INODE_REGION_OFFSET + 2 * SECTOR_BYTES, 25);
        // fs.disk.print(INODE_REGION_OFFSET + 3 * SECTOR_BYTES, 25);

        let inode = fs.find("/dira/dirb/filec").unwrap();
        assert_eq!(inode, filec.inode);
    }
}