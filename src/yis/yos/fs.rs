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
    FileTooLarge,
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

    fn read_inode(&self, inum: usize) -> Result<INode, FileSystemError> {
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

        self.write_bmap(inode.inum, 1 * BLOCK_BYTES, true);

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

    fn create_dir(&mut self, parent: Option<(&INode, String)>) -> Res<INode> {
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


    // pub fn create_root(&mut self) -> Res<()> {
    //     // let inode = INode {
    //     //     size: 0,
    //     //     num_block: 1,
    //     //     blocks: [0; 15], 
    //     //     mode: INodeMode::Directory,
    //     // };
    //     // let mut block_buf = [0; 4*1024];
    //     // let mut i = 0;
    //     // let mut off = 0;
    //     // let mut block_addr = 0;
    //     // write_as_words(&mut block_buf, off, inode.size);
    //     // off += 8;
    //     // block_buf[off] = inode.num_block;
    //     // off += 1;
    //     // for i in 0..inode.num_block {
    //     //     write_as_words(&mut block_buf, off + i * 8, inode.blocks[i as usize]);
    //     // }
    //     // off += 15 * 8;
    //     // block_buf[off] = match inode.mode {
    //     //     INodeMode::RegularFile => 0,
    //     //     INodeMode::Directory => 1,
    //     // };
    //     // off += 1;
    //     // self.disk.write_block(block_addr, &mut block_buf)
    //     //     .ok_or(FileSystemError::BadBlockAddr)?;
    //     Ok(())
    // }



    // fn read(&self, file: &mut File, buf: &mut [u8], len: usize) -> Res<usize> {
    //     if file.inode.mode != INodeMode::Directory {
    //         return Err(FileSystemError::InvalidINodeMode);
    //     }

    //     let mut block_buf = [0; 4*1024];
    //     let inode = &file.inode;
    //     let mut i = 0;
    //     let mut off = file.off; 
    //     for iblock in 0..inode.num_block {
    //         let block_addr = inode.blocks[iblock as usize] as usize;
    //         self.disk.read_block(block_addr, &mut block_buf)
    //             .ok_or(FileSystemError::BadBlockAddr)?;
    //         while i < len && off < (1+iblock as usize)*4*1024 {
    //             buf[i] = block_buf[off  % (4*1024)];
    //             i += 1;
    //             off += 1;
    //         }
    //     }
    //     file.off = off;
    //     Ok(len)
    // }

    // fn write(&mut self, file: &mut File, buf: &[u8], len: usize) -> Res<usize> {
    //     if file.inode.mode != INodeMode::Directory {
    //         return Err(FileSystemError::InvalidINodeMode);
    //     }
    //     let mut block_buf = [0; 4*1024];
    //     let inode = &file.inode;
    //     let mut i = 0;
    //     let mut off = file.off; 
    //     for iblock in 0..inode.num_block {
    //         while i < len && off < (1+iblock as usize)*4*1024 {
    //             block_buf[off  % (4*1024)] = buf[i];
    //             i += 1;
    //             off += 1;
    //         }

    //         let block_addr = inode.blocks[iblock as usize] as usize;
    //         self.disk.write_block(block_addr, &mut block_buf)
    //             .ok_or(FileSystemError::BadBlockAddr)?;
    //     }
    //     file.off = off;
    //     Ok(len)        
    // }

    // fn get_directory(&self, inode: &INode) -> Res<Directory> {
    //     if inode.mode != INodeMode::Directory {
    //         return Err(FileSystemError::InvalidINodeMode);
    //     }

    //     let mut block_buf = [0u8; BLOCK_BYTES];
    //     let block_addr = inode.blocks[0] as usize;
    //     self.disk.read_block(block_addr, &mut block_buf);

    //     let mut i = 0;
    //     let mut dirents = vec![];
    //     while block_buf[i] != 0x0 {
    //         let inum = read_as_words(&block_buf, i) as usize;
    //         i += 8;
    //         let name_len = block_buf[i] as usize;
    //         i += 1;
    //         let mut name = [0u8; 16];
    //         for j in 0..16 {
    //             name[j] = block_buf[i + j]
    //         }
    //         i += 16;
    //         let name = &name[..name_len];
    //         let name = String::from_utf8(name.to_vec())
    //             .map_err(|_| FileSystemError::FileNotFound)?; // FIXME: error handling
    //         dirents.push(Dirent {inum, name});
    //     }
    //     Ok(Directory {dirents})
    // }

    // fn find_inode(&self, inum: usize) -> Result<INode, FileSystemError> {
    //     let mut block_buf = [0; 4*1024];

    //     self.disk.read_block(INODE_REGION_OFFSET + inum / 16, &mut block_buf)
    //        .ok_or(FileSystemError::FileNotFound)?;
    //     let offset = 256 * (inum % 16);
    //     let size = read_as_words(&block_buf, offset);
    //     let num_block = block_buf[offset + 8];
    //     let mut blocks = [0; 15];
    //     for i in 0..num_block {
    //         let i = i as usize;
    //         blocks[i] = read_as_words(&block_buf, offset + 9 + i * 8);
    //     }
    //     let mode = block_buf[offset + 9 + 15 * 8];
    //     let mode = match mode {
    //         0 => INodeMode::RegularFile,
    //         1 => INodeMode::Directory,
    //         _ => return Err(FileSystemError::InvalidINodeMode),
    //     };
    //     Ok(INode {size, num_block, blocks, mode})
    // }

    // fn find_by_name_under_dir(&self, dir: &Directory, name: &str) -> Result<INode, FileSystemError> {
    //     // let dir = self.get_directory(inode)?;
    //     if let Some(i) = name.find("/") {
    //         // dir
    //         let dir_name = &name[..i];
    //         let resid_name = &name[(i+1)..];
    //         let dirent = dir.dirents.iter().find(|d| d.name.eq(dir_name))
    //             .ok_or(FileSystemError::FileNotFound)?;
    //         let inode = self.find_inode(dirent.inum as usize)?;
    //         let dir = &self.get_directory(&inode)?;
    //         return self.find_by_name_under_dir(dir, resid_name)
    //     } else {
    //         // file
    //         let dirent = dir.dirents.iter().find(|d| d.name.eq(name))
    //             .ok_or(FileSystemError::FileNotFound)?;
    //         let inode = self.find_inode(dirent.inum as usize)?;
    //         Ok(inode)
    //     }
    // }

    // fn find_by_name(&self, name: &str) -> Result<INode, FileSystemError> {
    //     // fetch root inode
    //     let root_inode = self.find_inode(INODE_REGION_OFFSET)?;
    //     let root_dir = self.get_directory(&root_inode)?;
    //     if let Some("/") = name.get(0..1) {
    //         let name = &name[1..];
    //         self.find_by_name_under_dir(&root_dir, name)
    //     } else {
    //         Err(FileSystemError::FileNotFound)
    //     }

    // }

    // fn create_regular_file(&mut self, dir_inum: usize, name: &str) -> Result<File, FileSystemError> {
    //     let inode = self.find_inode(dir_inum)?;
    //     let dir = &mut self.get_directory(&inode)?;
    //     let new_inum = 1; // FIXME: allocate free block
    //     dir.dirents.push(Dirent {inum: new_inum, name: String::from(name)});

    //     let inode = INode {
    //         size: 0,
    //         num_block: 1,
    //         blocks: [0; 15], 
    //         mode: INodeMode::RegularFile,
    //     };
    //     // FIXME: save inode and dir to disk

    //     let file = File { inode, off:0 };
    //     Ok(file)
    // }

}

pub struct File {
    inode: INode,
    off: usize,
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
struct INode {
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

// #[derive(Clone, Copy)]
// struct Block {
//     bytes: [u8; 4*1024]  // 4K byte per block
// }
// impl Block {
//     pub fn new() -> Block {
//         Block {
//             bytes: [0; 4*1024]
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

    // #[test]
    // fn test_read_write() {
    //     let mut fs = FileSystem::new();
    //     let mut file = fs.create_regular_file(0, "test.txt").unwrap();
    //     let buf = "hello world".as_bytes();
    //     let len = buf.len();
    //     fs.write(&mut file, buf, len).unwrap();
    //     let mut buf = [0; 1024];
    //     fs.read(&mut file, &mut buf, len).unwrap();
    //     let buf = String::from_utf8(buf.to_vec()).unwrap();
    //     assert_eq!(buf, "hello world");
    // }
}