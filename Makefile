note_trainer:
	dune build note_trainer.exe

all:
	dune build

.PHONY: test-expect
test-expect:
	expect test/expect/test1.exp
	expect test/expect/test2.exp


.PHONY: test
test: test-expect
