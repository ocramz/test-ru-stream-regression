all:
	stack build
	stack test

run:
	stack exec ru-app -- 3
