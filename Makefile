.PHONY: build clean test test-striped test-split test-lin-striped test-lin-split test-stm-striped test-stm-split bench

build:
	dune build

clean:
	dune clean

# Manual tests
test-striped:
	dune exec test/test_striped.exe

test-split:
	dune exec test/test_split_ordered.exe

# QCheck-Lin tests
test-lin-striped:
	dune exec test/qcheck_lin_striped.exe

test-lin-split:
	dune exec test/qcheck_lin_split_ordered.exe

# QCheck-STM tests
test-stm-striped-seq:
	dune exec test/qcheck_stm_striped.exe -- sequential

test-stm-striped-conc:
	dune exec test/qcheck_stm_striped.exe -- concurrent

test-stm-split-seq:
	dune exec test/qcheck_stm_split_ordered.exe -- sequential

test-stm-split-conc:
	dune exec test/qcheck_stm_split_ordered.exe -- concurrent

# Run all tests
test: test-striped test-split test-lin-striped test-lin-split test-stm-striped-seq test-stm-striped-conc test-stm-split-seq test-stm-split-conc

# Benchmarks
bench:
	dune exec bench/benchmark.exe
