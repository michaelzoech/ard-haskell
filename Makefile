
.PHONY: compile
compile:
	stack build

.PHONY: all
all: compile run

.PHONY: test
test:
	stack test

.PHONY: run
run:
	stack exec ard scenes/simple.scene

.PHONY: clean
clean:
	stack clean

.PHONY: doc
doc:
	stack haddock --open

