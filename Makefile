
.PHONY: all
all:
	stack build

.PHONY: test
test:
	stack test

.PHONY: run
run:
	stack exec ard

.PHONY: clean
clean:
	stack clean

