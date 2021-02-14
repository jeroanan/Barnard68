all: README.md docs/wikipedia.md

README.md: scribblings/README.scrbl
	scribble --dest . --markdown scribblings/README.scrbl

docs/wikipedia.md: scribblings/wikipedia.scrbl
	scribble --dest docs/ --markdown scribblings/wikipedia.scrbl

clean:
	rm -rf docs

$(shell mkdir -p docs)

