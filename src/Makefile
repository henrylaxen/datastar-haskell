BASE=/home/henry/haskell/datastar-haskell

.PHONY:	
test:
	pushd $(BASE) ; cabal repl  exe:datastar-demo --with-compiler=doctest 2>&1 | tee /tmp/sse.log ; popd
#	pushd $(BASE) ; cabal repl  exe:datastar-demo --with-compiler=doctest --repl-options=--verbose 2>&1 | tee /tmp/e.log
.PHONY:
watch:
	pushd  $(BASE) ; ghciwatch --clear ; popd
.PHONY:
build:
	pushd  $(BASE) ; cabal build  ; popd
.PHONY:
clean:
	pushd  $(BASE) ; rm -rf dist-newstyle ; popd
.PHONY:
tags:
	pushd  $(BASE)/src ; find . -name "*.hs" | grep -v "dist-newstyle" | xargs ghc-tags --etags -f TAGS ; popd
.PHONY:
run:
	pushd  $(BASE) ; cabal run 2>&1 | tee /tmp/sse.log ; popd

