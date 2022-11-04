
FORCE:

build:
	stack build

tests:
	stack test

prod: build tests
	git commit -a
	git push
