
%{
    #include <iostream>
    #include <string>

    #include "Token.h"

    void yyerror(const char* msg);

    // lex forward declarations
    extern "C" int yylex();
    extern int yylineno;
    extern char* yytext;
    extern FILE *yyin;
%}

%locations

%union {
  int num;
  Token* token;
} 

%start start     /* denotes the starting rule */

/* ========================= TOKENS ========================= */

/* Keywords */
%token PROGRAM FUNCTION PROCEDURE BEGIN_ END_ 
%token IF THEN ELSE DO WHILE
%token VAR OF

/* Syntactical symbols */
%token COMMA COLON SEMICOLON DOT RANGE_DOTS
%token BRACKETS_OPEN BRACKETS_CLOSING SQUARE_OPEN SQUARE_CLOSING

/* Data types */
%token INTEGER REAL BOOLEAN ARRAY

/* Operators */
%token OP_ASSIGNMENT 
%token OP_NOT
%token OP_EQUALS OP_NOT_EQUALS OP_LESS OP_LESS_EQUAL OP_GREATER OP_GREATER_EQUAL
%token OP_ADD OP_SUB OP_MUL OP_DIV OP_INTEGER_DIV
%token OP_AND OP_OR

/* Literals */
%token LITERAL_INTEGER LITERAL_REAL LITERAL_STRING LITERAL_TRUE LITERAL_FALSE

/* Identifier */
%token IDENTIFIER

%%

/* =========================================================== */
/* ==================== Program grammar ====================== */
/* =========================================================== */

start       : PROGRAM IDENTIFIER SEMICOLON 
              varDec subProgList compStmt DOT                
            ;

varDec      : VAR varDecList                                 
            | /* epsilon */                                  
            ;

varDecList  : varDecList identListType SEMICOLON             
            | identListType SEMICOLON                        
            ;

identListType : identList COLON type                         
              ;

identList   : identList COMMA IDENTIFIER                     
            | IDENTIFIER                                     
            ;

type        : simpleType                                     
            | ARRAY SQUARE_OPEN 
              LITERAL_INTEGER RANGE_DOTS LITERAL_INTEGER
              SQUARE_CLOSING OF simpleType                   
            ;

simpleType  : INTEGER
            | REAL
            | BOOLEAN
            ;

/* =========================================================== */
/* ===================== Method grammar ====================== */
/* =========================================================== */

subProgList : subProgList subProgHead varDec compStmt SEMICOLON
            | /* epsilon */                                    
            ;

subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON    
            | PROCEDURE IDENTIFIER args SEMICOLON              
            ;

args        : BRACKETS_OPEN parList BRACKETS_CLOSING           
            | /* epsilon */                                    
            ;

parList     : parList SEMICOLON identListType                  
            | identListType                                    
            ;


/* =========================================================== */
/* =================== Statement grammar ===================== */
/* =========================================================== */

statement   : procCall                                         
            | assignStmt                                       
            | compStmt                                         
            | ifStmt                                           
            | whileStmt                                        
            ;

procCall    : IDENTIFIER                                       
            | IDENTIFIER params                                
            ;
params      : BRACKETS_OPEN exprList BRACKETS_CLOSING          
            ;


assignStmt  : IDENTIFIER OP_ASSIGNMENT expr                    
            | IDENTIFIER index OP_ASSIGNMENT expr              
            ;
index       : SQUARE_OPEN expr SQUARE_CLOSING                  
            | SQUARE_OPEN expr RANGE_DOTS expr SQUARE_CLOSING   /* TODO: fix */
            ;


compStmt    : BEGIN_ stmtList END_                             
            ;
stmtList    : stmtList SEMICOLON statement                     
            | statement                                        
            ;


ifStmt      : IF expr THEN statement                           
            | IF expr THEN statement ELSE statement            
            ;

whileStmt   : WHILE expr DO statement                          
            ;

/* =========================================================== */
/* =================== Expression grammar ==================== */
/* =========================================================== */

expr        : simpleExpr relOp simpleExpr                      
            | simpleExpr                                       
            ;

simpleExpr  : simpleExpr addOp term                            
            | term                                             
            ;
    
term        : term mulOp factor                                
            | factor                                           
            ;

factor      : LITERAL_INTEGER                                  
            | LITERAL_REAL                                     
            | LITERAL_STRING                                   
            | LITERAL_TRUE                                     
            | LITERAL_FALSE                                    

            | IDENTIFIER                                       
            | IDENTIFIER SQUARE_OPEN expr SQUARE_CLOSING       
            | IDENTIFIER BRACKETS_OPEN exprList BRACKETS_CLOSING

            | OP_NOT factor                                    
            | OP_SUB factor                                    

            | BRACKETS_OPEN expr BRACKETS_CLOSING              
            ;

relOp       : OP_EQUALS                                        
            | OP_NOT_EQUALS                                    
            | OP_LESS                                          
            | OP_LESS_EQUAL                                    
            | OP_GREATER                                       
            | OP_GREATER_EQUAL                                 
            ;

addOp       : OP_ADD                                           
            | OP_SUB                                           
            | OP_OR                                            
            ;

mulOp       : OP_MUL                                           
            | OP_DIV                                           
            | OP_INTEGER_DIV                                   
            | OP_AND                                           


exprList    : exprList COMMA expr                              
            | expr                                             
            ;



%%

int main (void)
{
  // start parser
  yyparse();
}

void yyerror(const char* msg)
{
  std::cerr << "Error at line " << yylineno << ": " << msg << "\n" << std::endl;
}