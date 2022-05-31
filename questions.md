Questions:
- why constants in symbol table and not just in the EXPR itself? create own entry for EACH constant (for example, multiple 1's as multiple entries?)
- how to handle return values (which are assignment statements in pascal)?
- n in line 132 of sample.pas
- struct tENTRY **par_list in line 47 of ast_symtab.h (should also not be called "declarations"?)
- inherited attribute symtable - should do second pass or use yacc mid-actions?
- Pascal global scope (i in is_prime, for example), how to link global scope