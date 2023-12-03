BIN=./target/debug/csapp

build: ${BIN}

${BIN}: ./src/*.rs
		cargo build

yas1: ${BIN} ./y86/first.ys
		${BIN} run ./y86/first.ys

yas2: ${BIN} ./y86/arithmetric.ys
		@echo "test 21*(7-3) = 84 = 0x54"
		${BIN} run ./y86/arithmetric.ys --log-level 0 --watch-memory-range 0:400

ycc1: ${BIN} ./y86/plus.yc
		@echo "test 2+4"
		${BIN} run ./y86/plus.yc --log-level 0

ycc2: ${BIN} ./y86/arithmetric.yc
		@echo "test (  2  + 4 ) * 6 - (9 / 2 - 2*3) - 5 = 33 = 0x21"
		${BIN} run ./y86/arithmetric.yc --log-level 0

ycc3: ${BIN} ./y86/eq.yc
		@echo "test (  2  + 4 ) == (1+2) * 2"
		${BIN} run ./y86/eq.yc --log-level 0

ycc4: ${BIN} ./y86/4var.yc
		@echo "test (  1  - 2 ) + (3 * 4) = 11 = 0xB"
		${BIN} run ./y86/4var.yc --log-level 0 #--watch-memory-range 0:64

ycc5: ${BIN} ./y86/5assign.yc
		@echo "test if statement. expect 4"
		${BIN} run ./y86/5assign.yc --log-level 0

ycc6: ${BIN} ./y86/6while.yc
		@echo "test while statement. expect 7"
		${BIN} run ./y86/6while.yc --log-level 0

ycc7: ${BIN} ./y86/7block.yc
		@echo "test block. expect=(1+3)*(3+3)=4*6=24=0x18"
		${BIN} run ./y86/7block.yc --log-level 0

ycc8: ${BIN} ./y86/8def.yc
		@echo "test def. expect 5"
		${BIN} run ./y86/8def.yc --log-level 0
