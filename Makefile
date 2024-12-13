.PHONE: help clean cloc ge/ast ge/cst gp all run-trials-gp run-trials-ge/ast run-trials-ge/cst run-trials-all

help:
	@echo "Admin Targets: clean, cloc"
	@echo "Simple Targets: ge/ast, ge/cst, gp, all"
	@echo "Multi-run Targets: run-trials-ge/ast, run-..., run-trials-all"

clean:
	stack clean

cloc:
	cloc . --exclude-lang=CSV,C/C++\ Header

ge/ast:
	stack run ge-ast-exe

ge/cst:
	stack run ge-cst-exe

gp:
	stack run gp-exe

all: ge/ast ge/cst gp

TRIALS ?= 25
run-trials-gp:
	@for i in $(shell seq 1 $(TRIALS)); do \
		echo "Running gp #$$i"; \
		TRIAL="$$i" QUIET=1 stack run gp-exe; \
	done

run-trials-ge/ast:
	@for i in $(shell seq 1 $(TRIALS)); do \
		echo "Running ge/ast #$$i"; \
		TRIAL="$$i" QUIET=1 stack run ge-ast-exe; \
	done

run-trials-ge/cst:
	@for i in $(shell seq 1 $(TRIALS)); do \
		echo "Running ge/cst #$$i"; \
		TRIAL="$$i" QUIET=1 stack run ge-cst-exe; \
	done

run-trials-all: run-trials-gp run-trials-ge/ast run-trials-ge/cst
