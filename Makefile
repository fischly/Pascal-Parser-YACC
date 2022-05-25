all: compile test-all

# Compilation
compile:
	bison -d calc.y
	flex calc.l
	g++ -o testcalc.o lex.yy.c calc.tab.c


# Tests
test-success:
	@echo 
	@echo "Testing correct file (shouldn't produce syntax error): "
	cat test-code/test.pas | ./testcalc.o

test-failure:
	@echo
	@echo "Testing wrong file (should produce syntax error): "
	cat test-code/test-wrong.pas | ./testcalc.o

test-all: test-success test-failure



# Cleanup
clean: 
	rm -f lex.yy.c calc.tab.c calc.tab.h testcalc.o parsed-tree-run1.pas parsed-tree-run2.pas
