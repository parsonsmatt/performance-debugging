PHONY=run-time run-data-con

run-time:
	stack build --profile && stack exec -- treap +RTS -s -p -RTS

run-data-con:
	stack build --profile && stack exec -- treap +RTS -s -hd -p -RTS

run-data-source:
	stack build --profile && stack exec -- treap +RTS -s -hc -p -RTS
