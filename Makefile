
testyacc:
	bison -d calc.y
	flex calc.l
	g++ -o testcalc.o lex.yy.c calc.tab.c -lfl

	cat test.txt | ./testcalc.o


clean: 
	rm lex.yy.c calc.tab.c calc.tab.h testcalc.o
