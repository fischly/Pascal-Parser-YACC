
testyacc:
	bison -d calc.y
	flex calc.l
	g++ -o testcalc lex.yy.c calc.tab.c -lfl

	cat test.txt | ./testcalc


clean: 
	rm lex.yy.c calc.tab.c calc.tab.h testcalc
