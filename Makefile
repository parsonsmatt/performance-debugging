PHONY .run

run:
	stack build --profile && stack exec -- treap +RTS -s -p -RTS
