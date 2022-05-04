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
%token BBEGIN 
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

%token POINT 
%token COMMA 
%token SEMICOLON 
%token DOUBLE_POINT 

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
%token BRACKET_CURLY_OPEN 
%token BRACKET_CURLY_CLOSE 

%token NUMBER 
%token IDENTIFIER 
%token ERROR

%%

start : PROGRAM IDENTIFIER SEMICOLON varDec subProgList compStmt POINT

varDec : VAR varDecList
        | 

varDecList  : identListType SEMICOLON varDecList
            | 

identListType : identList DOUBLE_POINT type

identList : IDENTIFIER 
            | IDENTIFIER COMMA identList

subProgList : subProgHead varDec compStmt SEMICOLON subProgList
            | 

varDecList  : identListType SEMICOLON varDecList
            |  

compStmt : BBEGIN stmtList END

stmtList    : statement
            | statement SEMICOLON stmtList


subProgHead : FUNCTION IDENTIFIER args DOUBLE_POINT type SEMICOLON
            | PROCEDURE IDENTIFIER args SEMICOLON
    

type : simpleType
     | ARRAY BRACKET_SQUARE_OPEN NUMBER POINT POINT NUMBER BRACKET_SQUARE_CLOSE OF simpleType

args : BRACKET_ROUND_OPEN parList BRACKET_ROUND_CLOSE
      | 

statement : procCall
        | assignStmt
        | compStmt
        | ifStmt
        | whileStmt

ifStmt : IF expr THEN statement elsePart

elsePart : ELSE statement
        | 

simpleType : INTEGER 
            | BOOLEAN
            | REAL

expr : simpleExpr
     | simpleExpr relOp simpleExpr 

simpleExpr : term
            | term addOp term 

term : factor
     | factor mulOp term


factor : NUMBER
        | FALSE
        | TRUE
        | IDENTIFIER
        | IDENTIFIER index
        | IDENTIFIER params
        | MINUS factor
        | BRACKET_ROUND_OPEN expr BRACKET_ROUND_CLOSE

params : BRACKET_ROUND_OPEN exprList BRACKET_ROUND_CLOSE

index : BRACKET_SQUARE_OPEN expr BRACKET_SQUARE_CLOSE 
      | BRACKET_SQUARE_OPEN expr POINT POINT expr BRACKET_SQUARE_CLOSE

exprList : expr
         | expr COMMA exprList
            

parList : identListType
        | identListType SEMICOLON parList

procCall : IDENTIFIER
        | IDENTIFIER params

whileStmt : WHILE expr DO statement

assignStmt : IDENTIFIER assignment

assignment : ASSIGN expr
            | index ASSIGN expr

relOp : GREATER
        | LESS
        | EQUAL_GREATER
        | EQUAL_LESS
        | EQUAL
        | NOT_EQUAL

addOp : PLUS 
        | MINUS
        | OR

mulOp : MULT 
        | DIV_MATH 
        | DIV_TEXT
        | DIV
        | AND

%%

void yyerror (char *s) {
    printf("! Error parsing tokens: %s", s);
    printf(" in Line %d, Position %d - %d\n", yylineno, position, position+yyleng);
    printf("\tCould not resolve [%s]\n", yytext);
}
