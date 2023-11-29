BIN=./target/debug/csapp

build: ${BIN}

${BIN}: ./src/*.rs
		cargo build

yas1: ${BIN} ./y86/first.ys
		${BIN} run ./y86/first.ys

yas2: ${BIN} ./y86/arithmetric.ys
		${BIN} run ./y86/arithmetric.ys

ycc1: ${BIN} ./y86/plus.yc
		${BIN} run ./y86/plus.yc
