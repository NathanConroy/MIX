
FORCE:

build:
	stack build

prod: build
	git commit -a
