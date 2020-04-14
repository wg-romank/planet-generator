all: build run

build:
	elm make src/Main.elm --output=main.js

opt:
	elm make src/Main.elm --optimize --output=main.js

run:
	firefox ${PWD}/index.html

.PHONY: build run
