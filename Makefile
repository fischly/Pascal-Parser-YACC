all: yacc lex gcc test-all
yacc:
	yacc -d parse.y
lex:
	lex parse.l
gcc:
	gcc -Wimplicit-function-declaration y.tab.c lex.yy.c parse.c -o output.o 

test-all : test_success test_error
test_success:
	./output.o < sample.pas
test_error:
	./output.o < err_sample.pas

clean:
	rm y.tab.c y.tab.h output.o lex.yy.c