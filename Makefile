BIN=./target/debug/csapp

build: ${BIN}

${BIN}: ./src/*.rs
		cargo build

yas1: ${BIN} ./y86/first.ys
		${BIN} run ./y86/first.ys

yas2: ${BIN} ./y86/arithmetric.ys
		@echo "test 21+(7-3) = 25 = 0x19"
		${BIN} run ./y86/arithmetric.ys --log-level 0 --watch-memory-range 0:400

ycc1: ${BIN} ./y86/plus.yc
		@echo "test 2+4"
		${BIN} run ./y86/plus.yc --log-level 0
