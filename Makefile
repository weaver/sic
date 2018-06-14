all: compile

compile:
	chibi-scheme compile.scm

assemble-test:
	gcc -O3 -fomit-frame-pointer -momit-leaf-frame-pointer -S test.c
	cat test.s

build-test:
	gcc -O3 -fomit-frame-pointer -momit-leaf-frame-pointer driver.c test.s -o test

install:
	brew install chibi-scheme
	open "http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf"
	open "http://synthcode.com/scheme/chibi/"

docker-shell:
	docker build -t sic:latest .
	docker run -it --rm -v $(shell pwd):/sic -w /sic sic:latest /bin/bash
