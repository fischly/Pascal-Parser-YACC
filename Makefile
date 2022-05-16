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
	cat test.pas | ./testcalc.o

test-failure:
	@echo
	@echo "Testing wrong file (should produce syntax error): "
	cat test-wrong.pas | ./testcalc.o

test-AST-printing:
	@echo
	@echo "Testing AST printing: "

	cat test.pas             | ./testcalc.o > parsed-tree-run1.pas
	cat parsed-tree-run1.pas | ./testcalc.o > parsed-tree-run2.pas

	@echo 
	@echo "Difference between run1 and run2 (if no difference, this means everything worked):"
	diff parsed-tree-run1.pas parsed-tree-run2.pas

test-all: test-success test-failure test-AST-printing



# Cleanup
clean: 
	rm -f lex.yy.c calc.tab.c calc.tab.h testcalc.o parsed-tree-run1.pas parsed-tree-run2.pas
