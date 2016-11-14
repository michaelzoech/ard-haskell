
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
	eval time stack exec ard scenes/ambient_occlusion.ard

.PHONY: clean
clean:
	stack clean

.PHONY: doc
doc:
	stack haddock --open

