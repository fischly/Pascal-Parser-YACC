
%{
    #include <iostream>
    #include <string>

    #include "AST/Token.h"
    #include "AST/ASTPrinter.h"
    #include "AST/ast_symtab.h"

    void yyerror(const char* msg);

    // lex forward declarations
    extern "C" int yylex();
    extern int yylineno;
    extern char* yytext;
    extern FILE *yyin;

    // variable to hold the expression AST temporarily
    N_PROG* result;

    // forward declarations
    N_PROG* create_RootProg(Token* progIdentifier, N_PROG* subProgList, N_STMT* compStmt);
    N_PROG* create_Prog(Token* progIdentifier, N_PROG* subProgList, N_STMT* compStmt);

    N_STMT* create_AssignmentStmt(Token* identifier, N_EXPR* indexExpr, N_EXPR* rhs);
    N_STMT* create_IfStmt(N_EXPR* condition, N_STMT* then_part, N_STMT* else_part);
    N_STMT* create_WhileStmt(N_EXPR* condition, N_STMT* body);
    N_STMT* create_CallStmt(Token* identifierToken, N_EXPR* par_list);
    N_STMT* create_Stmt(N_STMT::eN_STMT_TYPE type, N_ASSIGN* assign_stmt, N_IF* if_stmt, N_WHILE* while_stmt, N_CALL* proc_call);
    void appendStmtToStmtList(N_STMT* stmtList, N_STMT* stmt);


    N_EXPR* create_LiteralExpr(Token* literalToken);
    N_EXPR* create_IdentifierExpr(Token* identifierToken, N_EXPR* indexExpr);
    N_EXPR* create_OPExpr(N_EXPR* left, Token* op, N_EXPR* right);
    N_EXPR* create_CallExpr(Token* funcToken, N_EXPR* parList);
    void appendExprToExprList(N_EXPR* exprList, N_EXPR* expr);
%}

%define parse.error verbose
%locations

%union {
    int num;
    Token* token;

    N_EXPR* expression;
    N_STMT* statement;
    N_PROG* program;
}

%start store     /* denotes the starting rule */

/* ========================= TOKENS ========================= */

/* Keywords */
%token PROGRAM FUNCTION PROCEDURE BEGIN_ END_
%token IF THEN ELSE DO WHILE
%token VAR OF

/* Syntactical symbols */
%token COMMA COLON SEMICOLON DOT RANGE_DOTS
%token <token> BRACKETS_OPEN BRACKETS_CLOSING SQUARE_OPEN SQUARE_CLOSING

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


%type <token> relOp addOp mulOp simpleType subProgHead
%type <expression> factor term simpleExpr expr index exprList params
%type <statement> statement procCall assignStmt compStmt ifStmt whileStmt stmtList
%type <program> subProgList start

%%


store       : start { result = $1; }


/* =========================================================== */
/* ==================== Program grammar ====================== */
/* =========================================================== */

start       : PROGRAM IDENTIFIER SEMICOLON 
              varDec subProgList compStmt DOT                 { $$ = create_RootProg($2, $5, $6); }
            ;

varDec      : VAR varDecList                                  { }
            | /* epsilon */                                   { }
            ;

varDecList  : varDecList identListType SEMICOLON              { }
            | identListType SEMICOLON                         { }
            ;

identListType : identList COLON type                          { }
              ;

identList   : identList COMMA IDENTIFIER                      { }
            | IDENTIFIER                                      { }
            ;

type        : simpleType                                      { }
            | ARRAY SQUARE_OPEN 
              LITERAL_INTEGER RANGE_DOTS LITERAL_INTEGER
              SQUARE_CLOSING OF simpleType                    { }
            ;

simpleType  : INTEGER
            | REAL
            | BOOLEAN
            ;


/* =========================================================== */
/* ===================== Method grammar ====================== */
/* =========================================================== */

subProgList : subProgList subProgHead varDec compStmt SEMICOLON { $$ = create_Prog($2, $1, $4); }
            | /* epsilon */                                     { $$ = NULL; }
            ;

subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON     { $$ = $2; /* for now only return a token for storing the identifier (TODO for next sheet) */  }
            | PROCEDURE IDENTIFIER args SEMICOLON               { $$ = $2; }
            ;

args        : BRACKETS_OPEN parList BRACKETS_CLOSING            { }
            | /* epsilon */                                     { }
            ;

parList     : parList SEMICOLON identListType                   { }
            | identListType                                     { }
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

procCall    : IDENTIFIER                                        { $$ = create_CallStmt($1, NULL); }
            | IDENTIFIER params                                 { $$ = create_CallStmt($1, $2); }
            ;
params      : BRACKETS_OPEN exprList BRACKETS_CLOSING           { $$ = $2; }
            ;


assignStmt  : IDENTIFIER OP_ASSIGNMENT expr                     { $$ = create_AssignmentStmt($1, NULL, $3); }
            | IDENTIFIER index OP_ASSIGNMENT expr               { $$ = create_AssignmentStmt($1, $2, $4); }
            ;
index       : SQUARE_OPEN expr SQUARE_CLOSING                   { $$ = $2; }
            | SQUARE_OPEN expr RANGE_DOTS expr SQUARE_CLOSING   { $$ = $2; }
            ;

compStmt    : BEGIN_ stmtList END_                              { $$ = $2; }
            ;
stmtList    : stmtList SEMICOLON statement                      { $$ = $1; appendStmtToStmtList($$, $3); }
            | statement                                         { $$ = $1; }
            ;


ifStmt      : IF expr THEN statement                            { $$ = create_IfStmt($2, $4, NULL); }
            | IF expr THEN statement ELSE statement             { $$ = create_IfStmt($2, $4, $6); }
            ;

whileStmt   : WHILE expr DO statement                           { $$ = create_WhileStmt($2, $4); }
            ;


/* =========================================================== */
/* =================== Expression grammar ==================== */
/* =========================================================== */

expr        : simpleExpr relOp simpleExpr                       { $$ = create_OPExpr($1, $2, $3); }
            | simpleExpr                                        { $$ = $1; }
            ;

simpleExpr  : simpleExpr addOp term                             { $$ = create_OPExpr($1, $2, $3); }
            | term                                              { $$ = $1; }
            ;

term        : term mulOp factor                                 { $$ = create_OPExpr($1, $2, $3); }
            | factor                                            { $$ = $1; }
            ;

factor      : LITERAL_INTEGER                                   { $$ = create_LiteralExpr($1); }
            | LITERAL_REAL                                      { $$ = create_LiteralExpr($1); }
            | LITERAL_STRING                                    { $$ = create_LiteralExpr($1); }
            | LITERAL_TRUE                                      { $$ = create_LiteralExpr($1); }
            | LITERAL_FALSE                                     { $$ = create_LiteralExpr($1); }

            | IDENTIFIER                                        { $$ = create_IdentifierExpr($1, NULL); }
            | IDENTIFIER SQUARE_OPEN expr SQUARE_CLOSING        { $$ = create_IdentifierExpr($1, $3); }

            | IDENTIFIER BRACKETS_OPEN exprList BRACKETS_CLOSING { $$ = create_CallExpr($1, $3); }

            | OP_NOT factor                                     { $$ = create_OPExpr($2, $1, NULL); }
            | OP_SUB factor                                     { $$ = create_OPExpr($2, $1, NULL); }

            | BRACKETS_OPEN expr BRACKETS_CLOSING               { $$ = create_OPExpr($2, new Token(yytokentype::BRACKETS_OPEN, "GROUP", 0), NULL); }
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


exprList    : exprList COMMA expr                               { $$ = $1; appendExprToExprList($$, $3); }
            | expr                                              { $$ = $1; } // { $$ = new std::vector<Expr::Expression*>(); $$->push_back($1); }
            ;



%%




/* =========================================================== */
/* ================ Method & Program helper ================== */
/* =========================================================== */
N_PROG* create_RootProg(Token* progIdentifier, N_PROG* subProgList, N_STMT* compStmt) {
    // create a new program (setting subProgList to NULL makes the function return a new program without appending it anywhere)
    N_PROG* prog = create_Prog(progIdentifier, NULL, compStmt);

    // append the given subProgList to the new "main" program
    prog->next = subProgList;

    return prog;
}

N_PROG* create_Prog(Token* progIdentifier, N_PROG* subProgList, N_STMT* compStmt) {
    N_PROG* prog = (N_PROG*) malloc(sizeof(N_PROG));

    prog->id = progIdentifier->lexeme;
    prog->stmt = compStmt;

    if (subProgList == NULL) {
        return prog;
    } else {
        prog->next = subProgList;
        /* while (subProgList->next != NULL) {
            subProgList = subProgList->next;
        }
        subProgList->next = prog; */

        return prog;
    }
}

/* =========================================================== */
/* =================== Statement helper ====================== */
/* =========================================================== */
N_STMT* create_AssignmentStmt(Token* identifier, N_EXPR* indexExpr, N_EXPR* rhs) {
    N_ASSIGN* assign_stmt = (N_ASSIGN*) malloc(sizeof(N_ASSIGN));

    assign_stmt->var_ref = (N_VAR_REF*) malloc(sizeof(N_VAR_REF));
    assign_stmt->var_ref->id = identifier->lexeme;
    assign_stmt->var_ref->index = indexExpr;

    assign_stmt->rhs_expr = rhs;

    return create_Stmt(N_STMT::_ASSIGN, assign_stmt, NULL, NULL, NULL);
}

N_STMT* create_IfStmt(N_EXPR* condition, N_STMT* then_part, N_STMT* else_part) {
    N_IF* if_stmt = (N_IF*) malloc(sizeof(N_IF));

    if_stmt->expr = condition;
    if_stmt->then_part = then_part;
    if_stmt->else_part = else_part;

    return create_Stmt(N_STMT::_IF, NULL, if_stmt, NULL, NULL);
}

N_STMT* create_WhileStmt(N_EXPR* condition, N_STMT* body) {
    N_WHILE* while_stmt = (N_WHILE*) malloc(sizeof(N_WHILE));

    while_stmt->expr = condition;
    while_stmt->stmt = body;

    return create_Stmt(N_STMT::_WHILE, NULL, NULL, while_stmt, NULL);
}

N_STMT* create_CallStmt(Token* identifierToken, N_EXPR* par_list) {
    N_CALL* call_stmt = (N_CALL*) malloc(sizeof(N_CALL));

    call_stmt->id = identifierToken->lexeme;
    call_stmt->par_list = par_list;

    return create_Stmt(N_STMT::_PROC_CALL, NULL, NULL, NULL, call_stmt);
}

N_STMT* create_Stmt(N_STMT::eN_STMT_TYPE type, N_ASSIGN* assign_stmt, N_IF* if_stmt, N_WHILE* while_stmt, N_CALL* proc_call) {
    N_STMT* stmt = (N_STMT*) malloc(sizeof(N_STMT));
    stmt->typ = type;

    switch (stmt->typ)
    {
        case N_STMT::_ASSIGN:    stmt->node.assign_stmt = assign_stmt; break;
        case N_STMT::_IF:        stmt->node.if_stmt = if_stmt; break;
        case N_STMT::_WHILE:     stmt->node.while_stmt = while_stmt; break;
        case N_STMT::_PROC_CALL: stmt->node.proc_call = proc_call; break;

        default: cout << "ERROR: got unexpected statement type!\n"; exit(1); break;
    }

    return stmt;
}

void appendStmtToStmtList(N_STMT* stmtList, N_STMT* stmt) {
    while (stmtList->next != NULL) {
        stmtList = stmtList->next;
    }

    stmtList->next = stmt;
}

/* =========================================================== */
/* =================== Expression helper ===================== */
/* =========================================================== */

N_EXPR* create_LiteralExpr(Token* literalToken) {
    N_EXPR* expr = (N_EXPR*) malloc(sizeof(N_EXPR));

    expr->typ = tN_EXPR::CONSTANT;
    expr->desc.constant = literalToken->lexeme;

    return expr;
}

N_EXPR* create_IdentifierExpr(Token* identifierToken, N_EXPR* indexExpr) {
    N_EXPR* expr = (N_EXPR*) malloc(sizeof(N_EXPR));

    expr->typ = tN_EXPR::VAR_REF;

    expr->desc.var_ref = (N_VAR_REF*) malloc(sizeof(N_VAR_REF));
    expr->desc.var_ref->id = identifierToken->lexeme;
    expr->desc.var_ref->index = indexExpr;

    return expr;
}

N_EXPR* create_OPExpr(N_EXPR* left, Token* op, N_EXPR* right) {
    N_EXPR* expr = (N_EXPR*) malloc(sizeof(N_EXPR));

    expr->typ = tN_EXPR::OP;

    expr->desc.operation.expr = left;
    expr->desc.operation.expr->next = right;
    /* expr->desc.operation.op = tN_EXPR::uN_EXPR_UNION::tN_OP::EQ_OP; // EQ_OP */
    expr->desc.operation.op = op; // EQ_OP

    return expr;
}

N_EXPR* create_CallExpr(Token* funcToken, N_EXPR* parList) {
    N_EXPR* expr = (N_EXPR*) malloc(sizeof(N_EXPR));

    expr->typ = tN_EXPR::FUNC_CALL;

    expr->desc.func_call = (N_CALL*) malloc(sizeof(N_CALL));
    expr->desc.func_call->id = funcToken->lexeme;
    expr->desc.func_call->par_list = parList;

    return expr;
}

void appendExprToExprList(N_EXPR* exprList, N_EXPR* expr) {
    while (exprList->next != NULL) {
        exprList = exprList->next;
    }

    exprList->next = expr;
}



/* =========================================================== */
/* ==================== Main entry point ===================== */
/* =========================================================== */

int main (void) {
    // start parser
    yyparse();

    // if no error occured, print the resulting AST
    if (result != NULL) {
        // from ASTPrinter.h
        printProgram(result);
    }
}

void yyerror(const char* msg) {
    std::cerr << "Error at line " << yylineno << ": " << msg << "\n" << std::endl;
}