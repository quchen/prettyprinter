readme :
	stack build wl-pprint:exe:readme-generator
	stack exec readme-generator
bench :
	mkdir -p generated
	stack bench wl-pprint --benchmark-arguments "--output ../generated/wl-pprint-benchmark.html"
doc :
	stack haddock --open wl-pprint
