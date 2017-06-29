help:
	@cat Makefile

build:
	stack build

clean:
	\rm -rf .stack-work

hlint:
	stack exec hlint .

setup:
	stack setup

watch:
	stack build --fast --file-watch

b: build
hl: hlint
i: install
r: run
s: setup
w: watch
