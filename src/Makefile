.PHONY:	
test:
	cd .. ; cabal repl sse --with-compiler=doctest 2>&1 | tee /tmp/sse.log
#	cd .. ; cabal repl sse --with-compiler=doctest --repl-options=--verbose 2>&1 | tee /tmp/e.log
.PHONY:
watch:
	cd .. ; ghciwatch --clear
.PHONY:
build:
	cd .. ; cabal build
.PHONY:
clean:
	cd .. ; rm -rf dist-newstyle
.PHONY:
tags:
	find . -name "*.hs" | xargs ghc-tags --etags -f TAGS
