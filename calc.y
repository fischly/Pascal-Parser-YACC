
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
%token INTEGER REAL BOOLEAN ARRAY

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

start       : PROGRAM IDENTIFIER SEMICOLON varDec subProgList compStmt DOT { std::cout << "start       : PROGRAM IDENTIFIER SEMICOLON varDec subProgList compStmt" << std::endl; $$ = new Program($2, $4, $5, static_cast<Stmt::Block*>($6)); }
            ;

varDec      : VAR varDecList                { std::cout << "varDec      : VAR varDecList" << std::endl; $$ = $2; }
            | /* epsilon */                 { std::cout << "varDec      : epsilon" << std::endl;        $$ = new std::vector<Variable*>(); }
            ;

varDecList  : varDecList identListType SEMICOLON { std::cout << "varDecList  : varDecList identListType SEMICOLON" << std::endl; $$ = $1; std::copy($2->begin(), $2->end(), std::back_inserter(*$$)); }
            | identListType SEMICOLON            { std::cout << "varDecList  : identListType" << std::endl;                      $$ = $1; }
            ;

identListType : identList COLON type        { std::cout << "identListType : identList COLON type" << std::endl;     $$ = new std::vector<Variable*>(); for (const auto token : *$1) { $$->push_back(new Variable(token, $3)); } }
              ;

identList   : identList COMMA IDENTIFIER    { std::cout << "identList   : identList COMMA IDENTIFIER" << std::endl; $$ = $1; $$->push_back($3); }
            | IDENTIFIER                    { std::cout << "identList   : IDENTIFIER" << std::endl;                 $$ = new std::vector<Token*>(); $$->push_back($1); }
            ;

type        : simpleType                    { std::cout << "type        : simpleType" << std::endl;                 $$ = new Variable::VariableTypeSimple($1); }
            | ARRAY SQUARE_OPEN 
              LITERAL_INTEGER RANGE_DOTS LITERAL_INTEGER
              SQUARE_CLOSING OF simpleType  { std::cout << "type        : complexType (shorted here)" << std::endl; $$ = new Variable::VariableTypeArray($8, $3, $5); }
            ;

simpleType  : INTEGER
            | REAL
            | BOOLEAN
            ;

/* =========================================================== */
/* ===================== Method grammar ====================== */
/* =========================================================== */

subProgList : subProgList subProgHead varDec compStmt SEMICOLON { std::cout << "subProgList : subProgList subProgHead varDec compStmt SEMICOLON" << std::endl; $$ = $1; $2->declarations = $3; $2->block = static_cast<Stmt::Block*>($4); $$->push_back($2); }
            | /* epsilon */                                     { std::cout << "subProgList : epsilon" << std::endl;                                           $$ = new std::vector<Method*>(); }
            ;

subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON { std::cout << "subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON" << std::endl; $$ = new Method($2, $3, nullptr, nullptr, $5); }
            | PROCEDURE IDENTIFIER args SEMICOLON           { std::cout << "subProgHead : PROCEDURE IDENTIFIER args SEMICOLON" << std::endl;           $$ = new Method($2, $3, nullptr, nullptr, nullptr); }
            ;

args        : BRACKETS_OPEN parList BRACKETS_CLOSING { std::cout << "args        : BRACKETS_OPEN parList BRACKETS_CLOSING" << std::endl; $$ = $2; }
            | /* epsilon */                          { std::cout << "args        : epsilon" << std::endl;                                $$ = new std::vector<Variable*>(); }
            ;

parList     : parList SEMICOLON identListType { std::cout << "parList     : parList SEMICOLON identListType" << std::endl; $$ = $1; std::copy($3->begin(), $3->end(), std::back_inserter(*$$)); }
            | identListType                   { std::cout << "parList     : identListType" << std::endl;                   $$ = $1; }
            ;


/* =========================================================== */
/* =================== Statement grammar ===================== */
/* =========================================================== */

statement   : procCall                      { std::cout << "statement   : procCall" << std::endl;   $$ = $1; }
            | assignStmt                    { std::cout << "statement   : assignStmt" << std::endl; $$ = $1; }
            | compStmt                      { std::cout << "statement   : compStmt" << std::endl;   $$ = $1; }
            | ifStmt                        { std::cout << "statement   : ifStmt" << std::endl;     $$ = $1; }
            | whileStmt                     { std::cout << "statement   : whileStmt" << std::endl;  $$ = $1; }
            ;

procCall    : IDENTIFIER                    { std::cout << "procCall    : IDENTIFIER" << std::endl;         $$ = new Stmt::Call($1, new std::vector<Expression*>()); }
            | IDENTIFIER params             { std::cout << "procCall    : IDENTIFIER params" << std::endl;  $$ = new Stmt::Call($1, $2); }
            ;
params      : BRACKETS_OPEN exprList BRACKETS_CLOSING { std::cout << "params      : BRACKETS_OPEN exprList BRACKETS_CLOSING" << std::endl; $$ = $2; }
            ;


assignStmt  : IDENTIFIER OP_ASSIGNMENT expr       { std::cout << "assignStmt  : IDENTIFIER OP_ASSIGNMENT expr" << std::endl;        $$ = new Stmt::Assignment($1, nullptr, $3); }
            | IDENTIFIER index OP_ASSIGNMENT expr { std::cout << "assignStmt  : IDENTIFIER index OP_ASSIGNMENT expr" << std::endl;  $$ = new Stmt::Assignment($1, $2, $4); }
            ;
index       : SQUARE_OPEN expr SQUARE_CLOSING                 { std::cout << "index       : SQUARE_OPEN expr SQUARE_CLOSING" << std::endl;            $$ = $2; }
            | SQUARE_OPEN expr RANGE_DOTS expr SQUARE_CLOSING { std::cout << "index       : SQUARE_OPEN expr RANGE_DOTS SQUARE_CLOSING" << std::endl; $$ = $2; } /* TODO: fix */
            ;


compStmt    : BEGIN_ stmtList END_                { std::cout << "compStmt    : BEGIN_ stmtList END_" << std::endl; $$ = new Stmt::Block($2); }
            ;
stmtList    : stmtList SEMICOLON statement        { std::cout << "stmtList    : stmtList SEMICOLON statement" << std::endl; $$ = $1; $$->push_back($3); } /* TODO: maybe error here in case of memory exception */
            | statement                           { std::cout << "stmtList    : statement" << std::endl;                    $$ = new std::vector<Stmt::Statement*>(); $$->push_back($1); }
            ;


ifStmt      : IF expr THEN statement                { std::cout << "ifStmt      : IF expr THEN statement" << std::endl;                 $$ = new Stmt::If($2, $4, nullptr); }
            | IF expr THEN statement ELSE statement { std::cout << "ifStmt      : IF expr THEN statement ELSE statement" << std::endl;  $$ = new Stmt::If($2, $4, $6); }
            ;

whileStmt   : WHILE expr DO statement              { std::cout << "whileStmt   : WHILE exprs DO statement" << std::endl; $$ = new Stmt::While($2, $4); }
            ;

/* =========================================================== */
/* =================== Expression grammar ==================== */
/* =========================================================== */

expr        : simpleExpr relOp simpleExpr   { std::cout << "expr        : simpleExpr relOp simpleExpr" << std::endl; $$ = new Expr::Binary($1, $2, $3); }
            | simpleExpr                    { std::cout << "expr        : simpleExpr" << std::endl;                  $$ = $1; }
            ;

simpleExpr  : simpleExpr addOp term         { std::cout << "simpleExpr  : simpleExpr addOp term" << std::endl; $$ = new Expr::Binary($1, $2, $3); }
            | term                          { std::cout << "simpleExpr  : term" << std::endl;                  $$ = $1; }
            ;
    
term        : term mulOp factor             { std::cout << "term        : term mulOp factor" << std::endl; $$ = new Expr::Binary($1, $2, $3); }
            | factor                        { std::cout << "term        : factor" << std::endl;            $$ = $1; }
            ;

factor      : LITERAL_INTEGER               { std::cout << "factor      : number" << std::endl;           $$ = new Expr::Literal($1); }
            | LITERAL_REAL                  { std::cout << "factor      : real" << std::endl;             $$ = new Expr::Literal($1); }
            | LITERAL_STRING                { std::cout << "factor      : string" << std::endl;           $$ = new Expr::Literal($1); }
            | LITERAL_TRUE                  { std::cout << "factor      : true" << std::endl;             $$ = new Expr::Literal($1); }
            | LITERAL_FALSE                 { std::cout << "factor      : false" << std::endl;            $$ = new Expr::Literal($1); }

            | IDENTIFIER                    { std::cout << "factor      : identifier" << $1 << std::endl; $$ = new Expr::Identifier($1, NULL); }
            | IDENTIFIER SQUARE_OPEN expr SQUARE_CLOSING  { std::cout << "factor      : identifier[exp]" << std::endl;  $$ = new Expr::Identifier($1, $3); }
            | IDENTIFIER BRACKETS_OPEN exprList BRACKETS_CLOSING { std::cout << "factor      : identifier(params)" << std::endl;  $$ = new Expr::Call($1, $3); }

            | OP_NOT factor                 { std::cout << "factor      : not factor" << std::endl;       $$ = new Expr::Unary($1, $2); }
            | OP_SUB factor                 { std::cout << "factor      : - factor" << std::endl;         $$ = new Expr::Unary($1, $2); }

            | BRACKETS_OPEN expr BRACKETS_CLOSING { std::cout << "factor      : brackets" << std::endl;         $$ = new Expr::Grouping($2); }
            ;

relOp       : OP_EQUALS                     { std::cout << "relOp       : '='" << std::endl;  }
            | OP_NOT_EQUALS                 { std::cout << "relOp       : '<>'" << std::endl;  }
            | OP_LESS                       { std::cout << "relOp       : '<'" << std::endl;  }
            | OP_LESS_EQUAL                 { std::cout << "relOp       : '<='" << std::endl;  }
            | OP_GREATER                    { std::cout << "relOp       : '>'" << std::endl;  }
            | OP_GREATER_EQUAL              { std::cout << "relOp       : '>='" << std::endl;  }
            ;

addOp       : OP_ADD                        { std::cout << "addOp       : '+'" << std::endl;  }
            | OP_SUB                        { std::cout << "addOp       : '-'" << std::endl;  }
            | OP_OR                         { std::cout << "addOp       : 'or'" << std::endl;  }
            ;

mulOp       : OP_MUL                        { std::cout << "mulOp       : '*'" << std::endl;  }
            | OP_DIV                        { std::cout << "mulOp       : '/'" << std::endl;  }
            | OP_INTEGER_DIV                { std::cout << "mulOp       : 'div'" << std::endl;  }
            | OP_AND                        { std::cout << "mulOp       : 'and'" << std::endl;  }


exprList    : exprList COMMA expr           { std::cout << "exprList    : exprList COMMA expr" << std::endl; $$ = new std::vector<Expr::Expression*>(); $$->insert($$->end(), $1->begin(), $1->end()); $$->push_back($3); }
            | expr                          { std::cout << "exprList    : expr" << std::endl; $$ = new std::vector<Expr::Expression*>(); $$->push_back($1); }
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