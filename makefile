
FORCE:

build:
	stack build

prod: build
	git commit -a
	git push
