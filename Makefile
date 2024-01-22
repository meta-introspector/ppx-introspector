
test/test2.txt: log.txt test/test2.ml 
	./_build/install/default/lib/ppx-introspector/ppx.exe   --impl test/test2.ml > test/test2.txt

test3/test3.txt:
	./_build/install/default/lib/ppx-introspector/ppx.exe   --impl test3/test.ml > test3/test3.txt


report: log.txt
	grep DEBUG2B: log.txt | cut -d: -f2- 
	grep DEBUG2A: log.txt | cut -d: -f2-

install:
	dune build -p        ppx-introspector -j 23 @install

build1: clean
	dune build

clean :
	dune clean	

log.txt:  src/ppx.ml
#	dune clean
	dune build --trace-file build.trace --verbose > log.txt 2>&1
         # --display=quiet
