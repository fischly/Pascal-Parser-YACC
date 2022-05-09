
testyacc:
	bison -d calc.y
	flex calc.l
	g++ -o testcalc.o lex.yy.c calc.tab.c

	@echo 
	@echo "Testing correct file (shouldn't produce syntax error): "
	cat test.pas | ./testcalc.o

	@echo
	@echo "Testing wrong file (should produce syntax error): "
	cat test-wrong.pas | ./testcalc.o


clean: 
	rm lex.yy.c calc.tab.c calc.tab.h testcalc.o
