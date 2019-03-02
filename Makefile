.PHONY: run
run:
	stack run

.PHONY: build
build:
	stack build

.PHONY: dist
dist:
	stack sdist
