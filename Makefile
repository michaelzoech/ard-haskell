
.PHONY: all
all:
	cabal build

.PHONY: test
test:
	cabal test --show-details=always --test-option=--color

.PHONY: run
run:
	cabal run

.PHONY: clean
clean:
	cabal clean

