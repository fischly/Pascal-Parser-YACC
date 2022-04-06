
%{
    #include <iostream>
    #include <string>

    #include "AST/Token.h"
    #include "AST/Expression.h"
    #include "AST/Visitors/AST2Text.h"


    void yyerror(const char* msg);

    // lex forward declarations
    extern "C" int yylex();
    extern int yylineno;
    extern char* yytext;
    extern FILE *yyin;

    // variable to hold the expression AST temporarily
    Expr::Expression* result;
%}

%union {int num; Token* token; Expr::Expression* expression; std::vector<Expr::Expression*>* expressionList; } 

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

%type <expression> factor term simpleExpr expr
%type <token> relOp addOp mulOp
%type <expressionList> exprList

%%
start       : expr                          { result = $1; }

/* =================== Expression grammar ==================== */

expr        : simpleExpr relOp simpleExpr   { std::cout << "expr        : simpleExpr relOp simpleExpr" << std::endl; $$ = new Expr::Binary($1, $2, $3); }
            | simpleExpr                    { std::cout << "expr        : simpleExpr" << std::endl; $$ = $1; }
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
  std::cerr << msg << std::endl;
}