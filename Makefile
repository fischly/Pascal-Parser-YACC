compile: bison flex gcc
bison:
	bison -d parse.y
flex:
	flex parse.l
gcc:
	g++ parse.cpp parse.tab.c lex.yy.c -o out.o


test-all: test-success test-error
test-success:
	./out.o < test/sample.pas
test-error:
	./out.o < test/err_sample.pas

sym:
	g++ testing_symtab.cpp 
	./a.out

all: compile test-all

clean:
	rm out.o parse.tab.c parse.tab.h lex.yy.c