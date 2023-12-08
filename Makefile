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

ycc2: ${BIN} ./y86/tests/arithmetric.yc
		@echo "test (  2  + 4 ) * 6 - (9 / 2 - 2*3) - 5 = 33 = 0x21"
		${BIN} run ./y86/tests/arithmetric.yc --log-level 2

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

ycc9: ${BIN} ./y86/9pointer.yc
		@echo "test pointer. expect 5"
		${BIN} run ./y86/9pointer.yc --log-level 0

ycc10: ${BIN} ./y86/10array.yc
		@echo "test pointer. expect 7"
		${BIN} run ./y86/10array.yc --log-level 0

ycc11: ${BIN} ./y86/tests/11global.yc
		@echo "test global variable. expect 0x0E"
		${BIN} run ./y86/tests/11global.yc --log-level 2

ycc12: ${BIN} ./y86/tests/12string.yc
		@echo "test string literal. expect 'e'=0x65"
		${BIN} run ./y86/tests/12string.yc --log-level 0

ycc13: ${BIN} ./y86/tests/13console.yc
		@echo "test console. expect 'Hello, World!' on console"
		${BIN} run ./y86/tests/13console.yc --log-level 0 --watch-memory-range 57344:57856

yas3: ${BIN} ./y86/tests/yas3_exception_div.ys
		${BIN} run ./y86/tests/yas3_exception_div.ys --log-level 2
	