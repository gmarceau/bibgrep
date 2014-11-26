BTPARSE_DIR = btparse-0.34
OCAML_DIR = /usr/lib/ocaml/                   # ${OCAML_DIR}/caml/ should contain mlvalue.h and other ocaml header files.
READLINE_INCLUDE_DIR = /usr/include/readline  # ${READLINE_INCLUDE_DIR}/readline.h should exists

# Other possible values, if the above doesn't work and you donwloaded all the dependencies yourself
# BTPARSE_DIR = ../btparse-0.33-modif
# OCAML_DIR = ../ocaml-3.06/stdlib     
# READLINE_INCLUDE_DIR = ../readline-4.4

.PHONY : bytecode native links test clean wc docs

native:  links ml-realpath.o ml-readline.o ml-btparse.o queryPar.ml queryLex.ml docs
	ocamlopt -o bibgrep -I . \
	 str.cmxa unix.cmxa \
	 localPrintf.ml localHashtbl.ml \
	 ptset.mli ptset.ml \
	 btparse.mli btparse.ml ml-btparse.o \
	 realpath.mli realpath.ml ml-realpath.o \
	 readline.ml ml-readline.o \
	 index.mli index.ml \
	 query.mli queryPar.mli queryLex.ml query.ml queryPar.ml \
	 engine.mli engine.ml main.ml \
	 -cclib libbtparse.a \
	 -cclib /usr/lib/libreadline.so \
	 -cclib /usr/lib/libncurses.so
	echo yey

bytecode:  links ml-realpath.o ml-readline.o ml-btparse.o queryPar.ml queryLex.ml docs
	ocamlc -g -custom -o bibgrep -I . \
	 str.cma unix.cma \
	 localPrintf.ml localHashtbl.ml \
	 ptset.mli ptset.ml \
	 btparse.mli btparse.ml ml-btparse.o \
	 realpath.mli realpath.ml ml-realpath.o \
	 readline.ml ml-readline.o \
	 index.mli index.ml \
	 query.mli queryPar.mli queryLex.ml query.ml queryPar.ml \
	 engine.mli engine.ml main.ml \
	 -cclib libbtparse.so \
	 -cclib /usr/lib/libreadline.so \
	 -cclib /usr/lib/libncurses.so
	echo yey

queryPar.ml: queryPar.mly
	ocamlyacc -v queryPar.mly 

queryLex.ml: queryLex.mll
	ocamllex queryLex.mll

ml-btparse.o: links ml-btparse.c
	gcc -I ${OCAML_DIR} -I . -I ${BTPARSE_DIR}/src -g -c ml-btparse.c 

ml-readline.o: links ml-readline.c
	gcc -I ${OCAML_DIR} -I . -I ${READLINE_INCLUDE_DIR} -g -c ml-readline.c 

ml-realpath.o: links ml-realpath.c
	gcc -I ${OCAML_DIR} -I . -g -c ml-realpath.c 

user-guide.pdf user-guide.ps: user-guide.tex
	latex \\nonstopmode\\input user-guide.tex
	dvips user-guide.dvi -o user-guide.ps
	ps2pdf user-guide.ps user-guide.pdf

programmer-guide.pdf programmer-guide.ps: programmer-guide.tex
	latex \\nonstopmode\\input programmer-guide.tex
	dvips programmer-guide.dvi -o programmer-guide.ps
	ps2pdf programmer-guide.ps programmer-guide.pdf

docs : user-guide.ps programmer-guide.ps

links : libbtparse.so libbtparse.a

libbtparse.so :
	ln -s ${BTPARSE_DIR}/src/.libs/libbtparse.so .
libbtparse.a :
	ln -s ${BTPARSE_DIR}/src/.libs/libbtparse.a .

test: native
	rm ~/.bibgrep.idx; true
	time ./bibgrep -v -sm 't.*:use.*' /home/rt/lib/bibtex/dl.bib 
	time ./bibgrep -v -sm " also have (- into // user )" /home/rt/lib/bibtex/dl.bib /home/rt/lib/bibtex/dsac.bib 
	time ./bibgrep -v --no-load --sort year /home/rt/lib/bibtex/*.bib -- - science user
	time ./bibgrep -v --sort year /home/rt/lib/bibtex/*.bib -- - science user
	time ./bibgrep -v --sort year /home/rt/lib/bibtex/*.bib -- user
	time ./bibgrep -v --no-load /home/rt/lib/bibtex/dl.bib -- user
	time ./bibgrep -v --sort year /home/rt/lib/bibtex/geom.bib -- -science user
	time ./bibgrep -v --sort year /home/rt/lib/bibtex/geom.bib -- science
	time ./bibgrep -v asd /sadwq/qwe.bib
	time ./bibgrep -v asd opaque.bib
	time ./bibgrep -v science /home/rt/lib/bibtex/*.bib
	time ./bibgrep -v --no-load science /home/rt/lib/bibtex/dl.bib ~/journal/*
	time ./bibgrep -v --list

clean:
	rm -f *.o *.cmo *.cmi *.cmx *.dvi *.ps *.pdf *.aux *.log *.output \
	queryPar.ml queryLex.ml bibgrep btparse.h

bibgrep-src-0.00.tgz : \
		Makefile \
		README \
		bibgrep.el \
		btparse-0.33.diff \
		btparse.ml \
		btparse.mli \
		engine.ml \
		engine.mli \
		index.ml \
		index.mli \
		journal.txt \
		libbtparse.a \
		libbtparse.so \
		localHashtbl.ml \
		localPrintf.ml \
		main.ml \
		ml-btparse.c \
		ml-readline.c \
		ml-readline.cc \
		ml-realpath.c \
		programmer-guide.tex \
		ptset.ml \
		ptset.mli \
		query.ml \
		query.mli \
		queryLex.mll \
		queryPar.mli \
		queryPar.mly \
		readline.ml \
		realpath.ml \
		realpath.mli \
		user-guide.roff \
		user-guide.tex \
		wordcnt.awk
	tar -czf bibgrep-src-0.00.tgz $^

bibgrep-0.00.tgz : bibgrep user-guide.pdf user-guide.roff
	tar -czf bibgrep-0.00.tgz $^

bibgrep-0.00-zip : bibgrep user-guide.pdf user-guide.roff
	zip bibgrep-0.00.zip $^

wc: clean
	cat *.ml *.mli *.mll *.mly *.c *.el | wc -l

