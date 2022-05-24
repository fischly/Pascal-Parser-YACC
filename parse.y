%{
    
#include <iostream>
#include <string>

void yyerror(const char *s);

extern int yylex();
extern int yylineno;
extern int position;
extern char* yytext;
extern int yyleng;

%}

%start start

%token PROGRAM 
%token VAR 
%token ARRAY 
%token OF 
%token FUNCTION 
%token PROCEDURE 
%token _BEGIN 
%token _END 
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

/* =========================================================== */
/* ==================== Program grammar ====================== */
/* =========================================================== */

start       : PROGRAM IDENT SEMICOLON varDec subProgList compStmt DOT                
            ;

varDec      : VAR varDecList                                 
            | /* epsilon */                                  
            ;

varDecList  : varDecList identListType SEMICOLON             
            | identListType SEMICOLON                        
            ;

identListType : identList COLON type                         
              ;

identList   : identList COMMA IDENT                     
            | IDENT                                     
            ;

type        : simpleType                                     
            | ARRAY BRACKET_SQUARE_OPEN NUM DOT DOT NUM BRACKET_SQUARE_CLOSE OF simpleType                   
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

subProgHead : FUNCTION IDENT args COLON type SEMICOLON    
            | PROCEDURE IDENT args SEMICOLON              
            ;

args        : BRACKET_ROUND_OPEN parList BRACKET_ROUND_CLOSE           
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

procCall    : IDENT                                       
            | IDENT params                                
            ;
params      : BRACKET_ROUND_OPEN exprList BRACKET_ROUND_CLOSE          
            ;


assignStmt  : IDENT ASSIGN expr                    
            | IDENT index ASSIGN expr              
            ;
index       : BRACKET_SQUARE_OPEN expr BRACKET_SQUARE_CLOSE                  
            | BRACKET_SQUARE_OPEN expr DOT DOT expr BRACKET_SQUARE_CLOSE   /* TODO: fix */
            ;


compStmt    : _BEGIN stmtList _END                             
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

factor      : NUM                                  
            | FALSE
            | TRUE
            | IDENT
            | IDENT index
            | IDENT params
            | MINUS factor
            | BRACKET_ROUND_OPEN expr BRACKET_ROUND_CLOSE            
            ;

relOp       : EQUAL                                        
            | NOT_EQUAL                                    
            | LESS                                          
            | EQUAL_LESS                                    
            | GREATER                                       
            | EQUAL_GREATER                                 
            ;

addOp       : PLUS                                           
            | MINUS                                           
            | OR                                            
            ;

mulOp       : MULT                                           
            | DIV_MATH                                           
            | DIV_TEXT                                   
            | AND                                           


exprList    : exprList COMMA expr                              
            | expr                                             
            ;


%%

void yyerror(const char* msg){
    printf("! Error parsing tokens: %s", msg);
    printf(" in Line %d, Position %d - %d\n", yylineno, position, position+yyleng);
    printf("\tCould not resolve [%s]\n", yytext);
}

// Where do you store the tree?