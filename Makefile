.PHONY: test

prog = ghc
path = /usr/bin/ghc-7.6.3

all: deps build install

tags:
	hasktags --etags --output='TAGS' *

build:
	cabal configure --with-$(prog)=$(path) && cabal build --with-$(prog)=$(path)

install:
	cabal install -w $(path)

deps:
	cabal install --only-dependencies -w $(path)

test: build
	cabal test
