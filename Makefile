all: build run

build:
	shelm make src/Main.elm --output=main.js

opt:
	shelm make src/Main.elm --optimize --output=main.js

run:
	firefox ${PWD}/index.html

.PHONY: build run
