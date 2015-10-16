targets : dist/index.html dist/elm.min.js dist/style.css

.PHONY : all
all : $(targets)

dist/% : %
	mkdir -p dist
	cp $< $@

elm.min.js : elm.js
	uglifyjs -m -c -o $@ $<

.PHONY : elm.js
elm.js :
	elm-make --warn src/Main.elm
