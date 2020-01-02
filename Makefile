UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FORMAT=-m64
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho
  CLANG_FORMAT=-m64
endif
endif

PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

main: main.ml compile.ml runner.ml sexp.ml
	$(BUILD) -package $(PKGS) main.native
	mv main.native main

test: compile.ml runner.ml test.ml sexp.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

output/%.run: output/%.o main.c
	clang -g $(CLANG_FORMAT) -o $@ main.c $<

output/%.o: output/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

output/%.s: input/%.adder main
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	ocamlbuild -clean
