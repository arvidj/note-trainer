note_trainer:
	dune build note_trainer.exe

all:
	dune build

test-expect:
	expect test/expect/test1.exp


test: note_trainer
	poetry run cram test/cram/test1.t
