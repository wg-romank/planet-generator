all: build run

build:
	elm make src/Main.elm --output=main.js

opt:
	elm make src/Main.elm --optimize --output=main_out.js
	uglifyjs main_out.js -c > main.js
	rm -f main_out.js

run:
	firefox ${PWD}/index.html

.PHONY: build run
