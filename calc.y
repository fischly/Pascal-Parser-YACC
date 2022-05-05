
%{
    #include <iostream>
    #include <string>

    #include "AST/Token.h"
    #include "AST/Expression.h"
    #include "AST/Statement.h"
    #include "AST/Variable.h"
    #include "AST/Method.h"
    #include "AST/Program.h"
    #include "AST/Visitors/AST2Text.h"


    void yyerror(const char* msg);

    // lex forward declarations
    extern "C" int yylex();
    extern int yylineno;
    extern char* yytext;
    extern FILE *yyin;

    // variable to hold the expression AST temporarily
    Program* result;
%}

%locations

%union {
  int num;
  Token* token;
  std::vector<Token*>* tokenList;

  Expr::Expression* expression;
  std::vector<Expr::Expression*>* expressionList;

  Stmt::Statement* statement;
  std::vector<Stmt::Statement*>* statementList;

  Variable::VariableType* variableType;
  std::vector<Variable*>* variableList;

  Method* method;
  std::vector<Method*>* methodList;

  Program* program;

} 

%start store     /* denotes the starting rule */

/* ========================= TOKENS ========================= */

/* Keywords */
%token PROGRAM FUNCTION PROCEDURE BEGIN_ END_ 
%token IF THEN ELSE DO WHILE
%token VAR OF

/* Syntactical symbols */
%token COMMA COLON SEMICOLON DOT RANGE_DOTS
%token BRACKETS_OPEN BRACKETS_CLOSING SQUARE_OPEN SQUARE_CLOSING

/* Data types */
%token <token> INTEGER REAL BOOLEAN ARRAY

/* Operators */
%token <token> OP_ASSIGNMENT 
%token <token> OP_NOT
%token <token> OP_EQUALS OP_NOT_EQUALS OP_LESS OP_LESS_EQUAL OP_GREATER OP_GREATER_EQUAL
%token <token> OP_ADD OP_SUB OP_MUL OP_DIV OP_INTEGER_DIV
%token <token> OP_AND OP_OR

/* Literals */
%token <token> LITERAL_INTEGER LITERAL_REAL LITERAL_STRING LITERAL_TRUE LITERAL_FALSE

/* Identifier */
%token <token> IDENTIFIER



/* ========================= TYPING ========================= */

%type <token> relOp addOp mulOp simpleType 
%type <expression> factor term simpleExpr expr index
%type <expressionList> exprList params
%type <statement> statement procCall assignStmt compStmt ifStmt whileStmt
%type <statementList> stmtList
%type <variableType> type
%type <variableList> varDec varDecList identListType parList args
%type <method> subProgHead
%type <methodList> subProgList
%type <tokenList> identList
%type <program> start

%%

store       : start { result = $1; }

/* =========================================================== */
/* ==================== Program grammar ====================== */
/* =========================================================== */

start       : PROGRAM IDENTIFIER SEMICOLON 
              varDec subProgList compStmt DOT                 { $$ = new Program($2, $4, $5, static_cast<Stmt::Block*>($6)); }
            ;

varDec      : VAR varDecList                                  { $$ = $2; }
            | /* epsilon */                                   { $$ = new std::vector<Variable*>(); }
            ;

varDecList  : varDecList identListType SEMICOLON              { $$ = $1; std::copy($2->begin(), $2->end(), std::back_inserter(*$$)); }
            | identListType SEMICOLON                         { $$ = $1; }
            ;

identListType : identList COLON type                          { $$ = new std::vector<Variable*>(); for (const auto token : *$1) { $$->push_back(new Variable(token, $3)); } }
              ;

identList   : identList COMMA IDENTIFIER                      { $$ = $1; $$->push_back($3); }
            | IDENTIFIER                                      { $$ = new std::vector<Token*>(); $$->push_back($1); }
            ;

type        : simpleType                                      { $$ = new Variable::VariableTypeSimple($1); }
            | ARRAY SQUARE_OPEN 
              LITERAL_INTEGER RANGE_DOTS LITERAL_INTEGER
              SQUARE_CLOSING OF simpleType                    { $$ = new Variable::VariableTypeArray($8, $3, $5); }
            ;

simpleType  : INTEGER
            | REAL
            | BOOLEAN
            ;

/* =========================================================== */
/* ===================== Method grammar ====================== */
/* =========================================================== */

subProgList : subProgList subProgHead varDec compStmt SEMICOLON { $$ = $1; $2->declarations = $3; $2->block = static_cast<Stmt::Block*>($4); $$->push_back($2); }
            | /* epsilon */                                     { $$ = new std::vector<Method*>(); }
            ;

subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON     { $$ = new Method($2, $3, nullptr, nullptr, $5); }
            | PROCEDURE IDENTIFIER args SEMICOLON               { $$ = new Method($2, $3, nullptr, nullptr, nullptr); }
            ;

args        : BRACKETS_OPEN parList BRACKETS_CLOSING            { $$ = $2; }
            | /* epsilon */                                     { $$ = new std::vector<Variable*>(); }
            ;

parList     : parList SEMICOLON identListType                   { $$ = $1; std::copy($3->begin(), $3->end(), std::back_inserter(*$$)); }
            | identListType                                     { $$ = $1; }
            ;


/* =========================================================== */
/* =================== Statement grammar ===================== */
/* =========================================================== */

statement   : procCall                                          { $$ = $1; }
            | assignStmt                                        { $$ = $1; }
            | compStmt                                          { $$ = $1; }
            | ifStmt                                            { $$ = $1; }
            | whileStmt                                         { $$ = $1; }
            ;

procCall    : IDENTIFIER                                        { $$ = new Stmt::Call($1, new std::vector<Expression*>()); }
            | IDENTIFIER params                                 { $$ = new Stmt::Call($1, $2); }
            ;
params      : BRACKETS_OPEN exprList BRACKETS_CLOSING           { $$ = $2; }
            ;


assignStmt  : IDENTIFIER OP_ASSIGNMENT expr                     { $$ = new Stmt::Assignment($1, nullptr, $3); }
            | IDENTIFIER index OP_ASSIGNMENT expr               { $$ = new Stmt::Assignment($1, $2, $4); }
            ;
index       : SQUARE_OPEN expr SQUARE_CLOSING                   { $$ = $2; }
            | SQUARE_OPEN expr RANGE_DOTS expr SQUARE_CLOSING   { $$ = $2; } /* TODO: fix */
            ;


compStmt    : BEGIN_ stmtList END_                              { $$ = new Stmt::Block($2); }
            ;
stmtList    : stmtList SEMICOLON statement                      { $$ = $1; $$->push_back($3); }
            | statement                                         { $$ = new std::vector<Stmt::Statement*>(); $$->push_back($1); }
            ;


ifStmt      : IF expr THEN statement                            { $$ = new Stmt::If($2, $4, nullptr); }
            | IF expr THEN statement ELSE statement             { $$ = new Stmt::If($2, $4, $6); }
            ;

whileStmt   : WHILE expr DO statement                           { $$ = new Stmt::While($2, $4); }
            ;

/* =========================================================== */
/* =================== Expression grammar ==================== */
/* =========================================================== */

expr        : simpleExpr relOp simpleExpr                       { $$ = new Expr::Binary($1, $2, $3); }
            | simpleExpr                                        { $$ = $1; }
            ;

simpleExpr  : simpleExpr addOp term                             { $$ = new Expr::Binary($1, $2, $3); }
            | term                                              { $$ = $1; }
            ;
    
term        : term mulOp factor                                 { $$ = new Expr::Binary($1, $2, $3); }
            | factor                                            { $$ = $1; }
            ;

factor      : LITERAL_INTEGER                                   { $$ = new Expr::Literal($1); }
            | LITERAL_REAL                                      { $$ = new Expr::Literal($1); }
            | LITERAL_STRING                                    { $$ = new Expr::Literal($1); }
            | LITERAL_TRUE                                      { $$ = new Expr::Literal($1); }
            | LITERAL_FALSE                                     { $$ = new Expr::Literal($1); }

            | IDENTIFIER                                        { $$ = new Expr::Identifier($1, NULL); }
            | IDENTIFIER SQUARE_OPEN expr SQUARE_CLOSING        { $$ = new Expr::Identifier($1, $3); }
            | IDENTIFIER BRACKETS_OPEN exprList BRACKETS_CLOSING { $$ = new Expr::Call($1, $3); }

            | OP_NOT factor                                     { $$ = new Expr::Unary($1, $2); }
            | OP_SUB factor                                     { $$ = new Expr::Unary($1, $2); }

            | BRACKETS_OPEN expr BRACKETS_CLOSING               { $$ = new Expr::Grouping($2); }
            ;

relOp       : OP_EQUALS                                         { }
            | OP_NOT_EQUALS                                     { }
            | OP_LESS                                           { }
            | OP_LESS_EQUAL                                     { }
            | OP_GREATER                                        { }
            | OP_GREATER_EQUAL                                  { }
            ;

addOp       : OP_ADD                                            { }
            | OP_SUB                                            { }
            | OP_OR                                             { }
            ;

mulOp       : OP_MUL                                            { }
            | OP_DIV                                            { }
            | OP_INTEGER_DIV                                    { }
            | OP_AND                                            { }


exprList    : exprList COMMA expr                               { $$ = $1; $$->insert($$->end(), $1->begin(), $1->end()); $$->push_back($3); }
            | expr                                              { $$ = new std::vector<Expr::Expression*>(); $$->push_back($1); }
            ;



%%

int main (void)
{
  // start parser
  yyparse();

  // initialize AST printer and start accepting the expression
  AST2Text ast2text;
  result->accept(&ast2text);

  // output printer result to stdout
  std::cout << ast2text.getResult() << std::endl;

}

void yyerror(const char* msg)
{
  std::cerr << "Error at line " << yylineno << ": " << msg << "\n" << std::endl;
}