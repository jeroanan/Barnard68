all: README.md docs/wikipedia.md docs/hackernews.md

README.md: scribblings/README.scrbl
	scribble --dest . --markdown scribblings/README.scrbl

docs/wikipedia.md: scribblings/wikipedia.scrbl
	scribble --dest docs/ --markdown scribblings/wikipedia.scrbl

docs/hackernews.md: scribblings/hackernews.scrbl
	scribble --dest docs/ --markdown scribblings/hackernews.scrbl

clean:
	rm -rf docs

$(shell mkdir -p docs)

