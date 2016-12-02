all :
	stack build --exec example
	stack test
	stack haddock
	mkdir -p generated
	stack bench --benchmark-arguments "-o generated/benchmark.html"
