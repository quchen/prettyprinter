readme :
	stack build wl-pprint:exe:readme-generator
	$(shell find wl-pprint -type f -executable -name "readme-generator")
bench :
	mkdir -p generated
	stack bench wl-pprint --benchmark-arguments "--output ../generated/wl-pprint-benchmark.html"
doc :
	stack haddock --open wl-pprint
