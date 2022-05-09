%{
#include <stdio.h>
#include <stdlib.h>

void yyerror(char *s);

extern int yylex();
extern int yylineno;
extern int position;
extern char* yytext;
extern int yyleng;

char* latestRule;

%}

%start start

%token PROGRAM 
%token VAR 
%token ARRAY 
%token OF 
%token FUNCTION 
%token PROCEDURE 
%token _BEGIN 
%token END 
%token WHILE 
%token DO 
%token THEN 
%token IF  
%token ELSE 

%token INTEGER 
%token REAL 
%token BOOLEAN 
%token TRUE 
%token FALSE 

%token EQUAL 
%token LESS 
%token GREATER 
%token EQUAL_GREATER 
%token EQUAL_LESS 
%token NOT_EQUAL 
%token ASSIGN 

%token DOT 
%token COMMA 
%token SEMICOLON 
%token COLON 

%token PLUS 
%token MINUS 
%token MULT 
%token DIV_MATH 
%token DIV_TEXT 
%token AND 
%token DIV
%token OR
%token MOD

%token BRACKET_ROUND_OPEN 
%token BRACKET_ROUND_CLOSE 
%token BRACKET_SQUARE_OPEN 
%token BRACKET_SQUARE_CLOSE 

%token NUM 
%token IDENT 
%token ERROR

%%

start   : PROGRAM IDENT SEMICOLON varDec subProgList compStmt DOT
        ;

varDec : VAR varDecList
        | 
        ; 

varDecList      : identListType SEMICOLON varDecList
                ;

identListType   : identList COLON type
                ;

identList       : IDENT 
                | IDENT COMMA identList
                ;

subProgList     : subProgHead varDec compStmt SEMICOLON subProgList
                | 
                ;

varDecList      : identListType SEMICOLON varDecList
                |  
                ;

compStmt        : _BEGIN stmtList END
                |

stmtList        : statement
                | statement SEMICOLON stmtList
                ;

subProgHead     : FUNCTION IDENT args COLON type SEMICOLON
                | PROCEDURE IDENT args SEMICOLON
                ;

type    : simpleType
        | ARRAY BRACKET_SQUARE_OPEN NUM DOT DOT NUM BRACKET_SQUARE_CLOSE OF simpleType
        ;

args    : BRACKET_ROUND_OPEN parList BRACKET_ROUND_CLOSE
        | 
        ;

statement       : procCall
                | assignStmt
                | compStmt
                | ifStmt
                | whileStmt
                ;

ifStmt  : IF expr THEN statement elsePart
        ;

elsePart        : ELSE statement
                | 
                ;

simpleType      : INTEGER 
                | BOOLEAN
                | REAL
                ;

expr    : simpleExpr
        | simpleExpr relOp simpleExpr 
        ;

simpleExpr      : term
                | simpleExpr addOp term 
                ;

term    : factor
        | term mulOp factor
        ;

factor  : NUM
        | FALSE
        | TRUE
        | IDENT
        | IDENT index
        | IDENT params
        | MINUS factor
        | BRACKET_ROUND_OPEN expr BRACKET_ROUND_CLOSE
        ;

params  : BRACKET_ROUND_OPEN exprList BRACKET_ROUND_CLOSE
        ;

index   : BRACKET_SQUARE_OPEN expr BRACKET_SQUARE_CLOSE 
        | BRACKET_SQUARE_OPEN expr DOT DOT expr BRACKET_SQUARE_CLOSE
        ;

exprList : expr
         | expr COMMA exprList
         ;   

parList : identListType
        | identListType SEMICOLON parList
        ;

procCall : IDENT
         | IDENT params
         ;

whileStmt       : WHILE expr DO statement
                ;

assignStmt      : IDENT assignment
                ;

assignment      : ASSIGN expr
                | index ASSIGN expr
                ;

relOp : GREATER
        | LESS
        | EQUAL_GREATER
        | EQUAL_LESS
        | EQUAL
        | NOT_EQUAL
        ;

addOp : PLUS 
        | MINUS
        | OR
        ;

mulOp : MULT 
        | DIV_MATH 
        | DIV_TEXT
        | DIV
        | AND
        ;

%%

void yyerror (char *s) {
    printf("! Error parsing tokens: %s", s);
    printf(" in Line %d, Position %d - %d\n", yylineno, position, position+yyleng);
    printf("\tCould not resolve [%s]\n", yytext);
}
