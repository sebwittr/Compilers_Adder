UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
endif
endif

PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind -cflag -annot

main: main.ml compile.ml runner.ml sexp.ml
	$(BUILD) -package $(PKGS) main.native
	mv main.native main

test: compile.ml runner.ml test.ml sexp.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

output/%.run: output/%.o main.c
	clang $(CLANG_FLAGS) -o $@ main.c $<

output/%.o: output/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/%.s
output/%.s: input/%.adder main
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	ocamlbuild -clean
