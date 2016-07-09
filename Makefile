all: empty-elm-dir
	# https://github.com/elm-lang/elm-make/issues/33
	export LANG=en_US.UTF-8 && elm make --yes src/Main.elm --output=public/index.html

deps:
	elm package install --yes
	npm install

empty-elm-dir:
	mkdir -p empty-elm-dir && cd empty-elm-dir && ../hacked-compiler/elm-make --yes


clean:
	rm -rf elm-stuff
	rm -rf node_modules
	rm public/elm.js
	rm -rf empty-elm-dir


loc:
	find src -regex ".*elm" | xargs wc -l

start: all
	node server.js
