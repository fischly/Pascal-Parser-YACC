
#pragma once

#include "Token.h"

/* Abstract Syntax Tree (AST) for Minipascal */

typedef enum { _FALSE=0, _TRUE } _BOOLEAN;
typedef enum { _BOOL=0, _INT, _REAL, _VOID } DATA_TYPE;


/* A program module is either the main program or a subprogram (function or procedure).
   A subprogram call is either a function call or a procedure call. */


/* The symbol table is implemented as a list of entries.
   Every program module has its own symbol table.
   The first element (entry) of a program module's symbol table is of type PROG and identifies the module itself.
   There are no (direct) references from one symbol table to an other. */


typedef struct tENTRY {

/* Type of entry: constant, scalar, array, program module, function or procedure call. */
  enum { _CONST=0, _VAR, _ARRAY, _PROG, _CALL } typ;


/* Data type (of a constant, variable, or function return value).
   VOID for main program or procedure. */
  DATA_TYPE data_type;

  union {
    char *id;		/* Variable or program module */
    int int_val; 	/* Value of a constant */
    float real_val;
    _BOOLEAN bool_val; 
  } base;


/* Extended information about an entry for an array, function or procedure */
  union {
    struct {			/* Lower and upper bound */
      int low;
      int upp;
    } bounds; 
    union { 
      struct tENTRY **par_list;	/* Declaration (i.e. in the first entry of its own symbol table): formal parameter list (vector of pointers to symbol table entries) */
      struct tN_PROG *ast;	/* Call (i.e. in the caller's symbol table): root node of the corresponding AST */
    } prog;
  } ext;
  struct tENTRY *next;		/* Next entry in the symbol table */
} ENTRY; 













/* Different types of syntax tree nodes */

/* 1. Reference to a (scalar or array) variable */
typedef struct tN_VAR_REF {
  char *id;			/* Id of variable */
  struct tN_EXPR *index;	/* One or two index expressions, null in case of scalar */
} N_VAR_REF;


/* 2. Expression */
typedef struct tN_EXPR {
  enum { CONSTANT=0, VAR_REF, OP, FUNC_CALL } typ;
  union uN_EXPR_UNION {
    char *constant;		/* String value of the constant */
    N_VAR_REF *var_ref;		/* Reference to a variable */
    struct tN_OP {
      struct tN_EXPR *expr;	/* One ore two operands (must not be null) */
      Token* op;
      // enum { NO_OP=0, EQ_OP, NE_OP, GT_OP, GE_OP, LT_OP, LE_OP, PLUS_OP, MINUS_OP, MULT_OP, SLASH_OP, DIV_OP, MOD_OP, AND_OP, OR_OP, NOT_OP } op; /* Operator */
    } operation;
    struct tN_CALL *func_call;	/* Function call */
  } desc;
  struct tN_EXPR *next;		/* Expression list */
} N_EXPR;


/* 3. Assignment statement */
typedef struct tN_ASSIGN {
  N_VAR_REF *var_ref;	/* Reference to a variable */
  N_EXPR *rhs_expr;	/* Right hand side expression */
} N_ASSIGN;


/* 4. If statement */
typedef struct tN_IF {
  N_EXPR *expr;
  struct tN_STMT *then_part;
  struct tN_STMT *else_part;
} N_IF;


/* 5. While statement */
typedef struct tN_WHILE {
  N_EXPR *expr;
  struct tN_STMT *stmt;
} N_WHILE;


/* 6. Function or procedure call */
typedef struct tN_CALL {
  char *id;		/* Id of function or procedure */
  N_EXPR *par_list;	/* Actual parameters */
} N_CALL;


/* 7. Statement */
typedef struct tN_STMT {
  enum eN_STMT_TYPE { _ASSIGN=0, _IF, _WHILE, _PROC_CALL } typ;
  union {
    N_ASSIGN *assign_stmt;
    N_IF *if_stmt;
    N_WHILE *while_stmt;
    N_CALL *proc_call;
  } node;
  struct tN_STMT *next;	/* Statement list */
} N_STMT;


/* 8. Root node of a program module's AST */
typedef struct tN_PROG {
  char* id;     /* TODO: remove later, just introduced to store the method name somewhere, as long as we should not use T_ENTRY */ 
  N_STMT *stmt;		/* First statement of the program module */
  struct tN_PROG *next;	/* Next program module in the list of modules */
} N_PROG;


/* Root node of the main program's AST */
// N_PROG * ast; 
