
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

    ENTRY* global_varDecl_symtab;  // symtab containing variable declarations of global scope (program scope)
    
    ENTRY* current_varDecl_symtab; // symtab containing variable declaration of the current function
    ENTRY* current_subProg_symtab; // symtab containing parameters of the current function

    METHOD* current_subProgList;   // list of all seen subprog entries to link them 

    // forward declarations
    N_PROG* create_RootProg(Token* progIdentifier, ENTRY* varDec, N_PROG* subProgList, N_STMT* compStmt);
    N_PROG* create_Prog(ENTRY* symtabMethodEntry, ENTRY* varDec, N_PROG* subProgList, N_STMT* compStmt);
    ENTRY* create_SubProgHead(Token* identifier, ENTRY* args, ENTRY* returnType);

    ENTRY* create_VarDecList(ENTRY* identListType, ENTRY* varDecList);
    ENTRY* create_IdentListType(ENTRY* identList, ENTRY* type);
    ENTRY* create_IdentList(Token* identifier, ENTRY* identList);
    ENTRY* create_TypeEntry(Token* typeToken, Token* tokenStart, Token* tokenEnd);

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

    void addMethodToGlobalScope(ENTRY* subProgEntry);

    ENTRY* getEntryFromSymtable(ENTRY* symtable, char* identifier);
    ENTRY* getEntryFromSymtable(ENTRY* varDeclSymtab, ENTRY* paramsSymtab, char* identifier);
    ENTRY* getEntryFromSymtableOrGlobal(ENTRY* symtable, char* identifier);
    ENTRY* getEntryFromGlobalScope(char* identifier);



    const char* DATA_TYPE_NAMES[] = { [0] = "_BOOL", [1] = "_INT", [2] = "_REAL", [3] = "_VOID" };
    const char* ENTRY_TYPE[] = { [0] = "_CONST", [1] = "_VAR", [2] = "_ARRAY", [3] = "_PROG", [4] = "_CALL" };

    void printVarDecl(ENTRY* decl) {
        while (decl != NULL) {
            std::cout << " - [" << ENTRY_TYPE[decl->typ] << "] " << decl->base.id << ": " << DATA_TYPE_NAMES[decl->data_type] << "\n";
            
            decl = decl->next;
        }
    }
    void printAllVariables(N_PROG* prog) {
        std::cout << "================= Printing all variables ==========================" << std::endl;
        std::cout << "---- Main program (" << prog->symtab_entry->base.id << ") ----" << std::endl;
        std::cout << " varDecl\n";

        printVarDecl(prog->symtab_entry->next);

        // other methods (sub programs)
        N_PROG* other = prog->next;
        while (other != NULL) {
            std::cout << "\n\n---- Method (" << other->symtab_entry->base.id << 
                ": " << DATA_TYPE_NAMES[other->symtab_entry->data_type] << 
                "[" << other->symtab_entry->ext.bounds.low << ".." << other->symtab_entry->ext.bounds.upp << "]" << 
                ") ----" << std::endl;
                
            std::cout << " arguments\n";
            ENTRY* arg = other->symtab_entry->ext.prog.par_list;
            while (arg != NULL) {
                std::cout << " - [" << ENTRY_TYPE[arg->typ] << "] " << arg->base.id << ": " << DATA_TYPE_NAMES[arg->data_type] << "\n";
                arg = arg->next;
            }

            std::cout << " varDecl\n";
            printVarDecl(other->symtab_entry->next);

            other = other->next;
        }

        std::cout << "================= Printing all variables DONE ==========================" << std::endl << std::endl;

    }


    void addToCurrentSubProgList(ENTRY* subProgHeadEntry) {
        METHOD* meth = (METHOD*) malloc(sizeof(METHOD));
        meth->subProg = subProgHeadEntry;

        if (current_subProgList == NULL) {
            current_subProgList = meth;
        } else {
            // append to the end
            METHOD* m = current_subProgList;
            while (m->next != NULL) {
                m = m->next;
            }
            m->next = meth;
        }
    }
%}

%define parse.error verbose
%locations

%union {
    int num;
    Token* token;

    N_EXPR* expression;
    N_STMT* statement;
    N_PROG* program;
    ENTRY*  entry;
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


%type <token>       relOp addOp mulOp simpleType 
%type <expression>  factor term simpleExpr expr index exprList params
%type <statement>   statement procCall assignStmt compStmt ifStmt whileStmt stmtList
%type <program>     subProgList start
%type <entry>       type identList identListType varDecList varDec parList args subProgHead

%%


store       : start { result = $1; }


/* =========================================================== */
/* ==================== Program grammar ====================== */
/* =========================================================== */

start       : PROGRAM IDENTIFIER SEMICOLON varDec             { global_varDecl_symtab = $4; }
              subProgList                                     { current_varDecl_symtab = $4; current_subProg_symtab = NULL; }  
              compStmt DOT                                    { $$ = create_RootProg($2, $4, $6, $8); printAllVariables($$); }
            ;

varDec      : VAR varDecList                                  { $$ = $2; }
            | /* epsilon */                                   { $$ = NULL; }
            ;

varDecList  : varDecList identListType SEMICOLON              { $$ = create_VarDecList($2, $1); }
            | identListType SEMICOLON                         { $$ = create_VarDecList($1, NULL); }
            ;

identListType : identList COLON type                          { $$ = create_IdentListType($1, $3); }
              ;

identList   : identList COMMA IDENTIFIER                      { $$ = create_IdentList($3, $1); }
            | IDENTIFIER                                      { $$ = create_IdentList($1, NULL); }
            ;

type        : simpleType                                      { $$ = create_TypeEntry($1, NULL, NULL); }
            | ARRAY SQUARE_OPEN 
              LITERAL_INTEGER RANGE_DOTS LITERAL_INTEGER
              SQUARE_CLOSING OF simpleType                    { $$ = create_TypeEntry($8, $3, $5); }
            ;

simpleType  : INTEGER
            | REAL
            | BOOLEAN
            ;


/* =========================================================== */
/* ===================== Method grammar ====================== */
/* =========================================================== */

subProgList : subProgList subProgHead varDec                    { current_varDecl_symtab = $3; current_subProg_symtab = $2; addMethodToGlobalScope(current_subProg_symtab); }
              compStmt SEMICOLON                                { $$ = create_Prog($2, $3, $1, $5); }
            | /* epsilon */                                     { $$ = NULL; }
            ;

subProgHead : FUNCTION IDENTIFIER args COLON type SEMICOLON     { $$ = create_SubProgHead($2, $3, $5); }
            | PROCEDURE IDENTIFIER args SEMICOLON               { $$ = create_SubProgHead($2, $3, NULL); }
            ;

args        : BRACKETS_OPEN parList BRACKETS_CLOSING            { $$ = $2; }
            | /* epsilon */                                     { $$ = NULL; }
            ;

parList     : parList SEMICOLON identListType                   { $$ = create_VarDecList($3, $1); }
            | identListType                                     { $$ = create_VarDecList($1, NULL); }
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
N_PROG* create_RootProg(Token* progIdentifier, ENTRY* varDec, N_PROG* subProgList, N_STMT* compStmt) {
    N_PROG* prog = (N_PROG*) malloc(sizeof(N_PROG));

    // initialize the first symtable entry, containing information about the program itself
    prog->symtab_entry = (ENTRY*) malloc(sizeof(ENTRY));
    prog->symtab_entry->typ = ENTRY::_PROG;
    prog->symtab_entry->data_type = DATA_TYPE::_VOID;
    prog->symtab_entry->base.id = progIdentifier->lexeme;
    prog->symtab_entry->next = varDec;

    // main body
    prog->stmt = compStmt;
    
    // append the given subProgList to the new "main" program
    prog->next = subProgList;

    return prog;
}

N_PROG* create_Prog(ENTRY* symtabMethodEntry, ENTRY* varDec, N_PROG* subProgList, N_STMT* compStmt) {
    N_PROG* prog = (N_PROG*) malloc(sizeof(N_PROG));

    prog->symtab_entry = symtabMethodEntry;
    prog->symtab_entry->next = varDec;

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

ENTRY* create_SubProgHead(Token* identifier, ENTRY* args, ENTRY* returnType) {
    ENTRY* methodEntry = (ENTRY*) malloc(sizeof(ENTRY));

    methodEntry->typ = ENTRY::_PROG;
    methodEntry->base.id = identifier->lexeme;
    methodEntry->ext.prog.par_list = args;

    if (returnType == NULL) {
        methodEntry->data_type = DATA_TYPE::_VOID;
    } else {
        methodEntry->data_type = returnType->data_type;

        if (returnType->typ == ENTRY::_ARRAY) {
            methodEntry->ext.bounds.low = returnType->ext.bounds.low;
            methodEntry->ext.bounds.upp = returnType->ext.bounds.upp;
        }
    }
    

    return methodEntry;
}

/* =========================================================== */
/* ===================== VarDec grammar ====================== */
/* =========================================================== */
ENTRY* create_VarDecList(ENTRY* identListType, ENTRY* varDecList) {
    if (varDecList == NULL) {
        return identListType;
    }

    // append identListType to end of varDecList
    ENTRY* varDecListIter = varDecList;
    while (varDecListIter->next != NULL) {
        varDecListIter = varDecListIter->next;
    }
    varDecListIter->next = identListType;

    return varDecList;
}


ENTRY* create_IdentListType(ENTRY* identList, ENTRY* type) {
    // update each identifiers entry datatype and bounds to that of the type entry
    ENTRY* identListIter = identList;
    while (identListIter != NULL) {
        identListIter->data_type = type->data_type;
        identListIter->ext.bounds.low = type->ext.bounds.low;
        identListIter->ext.bounds.upp = type->ext.bounds.upp;
        identListIter->typ = type->typ;

        identListIter = identListIter->next;
    }

    return identList;
}

ENTRY* create_IdentList(Token* identifier, ENTRY* identList) {
    ENTRY* entry = (ENTRY*) malloc(sizeof(ENTRY));
    
    entry->base.id = identifier->lexeme;

    if (identList == NULL) {
        return entry;
    }

    // append identifier to end of identList
    ENTRY* last = identList;
    while (last->next != NULL) {
        last = last->next;
    }
    last->next = entry;

    return identList;
}

ENTRY* create_TypeEntry(Token* typeToken, Token* tokenStart, Token* tokenEnd) {
    ENTRY* entry = (ENTRY*) malloc(sizeof(ENTRY));

    switch (typeToken->type) {
        case yytokentype::INTEGER: entry->data_type = DATA_TYPE::_INT; break;
        case yytokentype::REAL:    entry->data_type = DATA_TYPE::_REAL; break;
        case yytokentype::BOOLEAN: entry->data_type = DATA_TYPE::_BOOL; break;
        default: std::cout << "[ERROR] got unexpected type (" << typeToken->lexeme << ")\n"; break;
    }

    if (tokenStart != NULL && tokenEnd != NULL) {
        entry->typ = ENTRY::_ARRAY;
        entry->ext.bounds.low = atoi(tokenStart->lexeme);
        entry->ext.bounds.upp = atoi(tokenEnd->lexeme);
    } else {
        entry->typ = ENTRY::_VAR;
    }

    return entry;
}


/* =========================================================== */
/* =================== Statement helper ====================== */
/* =========================================================== */
N_STMT* create_AssignmentStmt(Token* identifier, N_EXPR* indexExpr, N_EXPR* rhs) {
    N_ASSIGN* assign_stmt = (N_ASSIGN*) malloc(sizeof(N_ASSIGN));

    assign_stmt->rhs_expr = rhs;

    assign_stmt->var_ref = (N_VAR_REF*) malloc(sizeof(N_VAR_REF));
    assign_stmt->var_ref->index = indexExpr;

    ENTRY* symtableEntry = NULL;

    // check if it's an assignment to the current function (= return statement)
    if (current_subProg_symtab != NULL) {
        if (strcmp(identifier->lexeme, current_subProg_symtab->base.id) == 0) {
            symtableEntry = current_subProg_symtab;

            std::cout << "[ASSIGNMENT STATEMENT] found return statement, variable lexeme = " << identifier->lexeme << ", referencing it to symtable " << current_subProg_symtab->base.id << "\n";
        }
    }

    // if it is not a return statement, check the symbol table for the variable that is assigned
    if (symtableEntry == NULL) {
        ENTRY* current_parameters_symtab = current_subProg_symtab != NULL ? current_subProg_symtab->ext.prog.par_list : NULL;
        symtableEntry = getEntryFromSymtable(current_varDecl_symtab, current_parameters_symtab, identifier->lexeme);
        
        std::cout << "[ASSIGNMENT STATEMENT] variable lexeme = " << identifier->lexeme << 
            ", current varDeclSymtab = " << (current_varDecl_symtab != NULL ? current_varDecl_symtab->base.id : "NULL") << 
            ", current argsSymtab = " << (current_parameters_symtab != NULL ? current_parameters_symtab->base.id : "NULL") << 
            ", found entry = " << (symtableEntry != NULL ? symtableEntry->base.id : "NULL") << 
            "\n";
    }

    // print error message if no symtable entry found
    if (symtableEntry == NULL) {
        std::cerr << "[ERROR] Using undeclared variable '" << identifier->lexeme << "' at line " << yylineno << "!\n";
    } 

    // link var_ref to the found symtable entry (or NULL)
    assign_stmt->var_ref->symtab_entry = symtableEntry;

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

    ENTRY* callEntry = getEntryFromGlobalScope(identifierToken->lexeme);
    std::cout << "[LINKING CALLEXPR '" << identifierToken->lexeme << "' to call entry of global table named " << 
        (callEntry != NULL ? callEntry->base.id : "NULL") << "\n";

    if (callEntry == NULL) {
        std::cerr << "[ERROR] Calling undeclared function '" << identifierToken->lexeme << "' at line " << yylineno << "!\n";
    }
    call_stmt->symtab_entry = callEntry;

    /* ENTRY* callEntry = getEntryFromGlobalScope(identifierToken->lexeme);
    std::cout << "[LINKING CALL STMT] linking call to " << identifierToken->lexeme << ", found callEntry: " << (callEntry != NULL ? callEntry->base.id : "NULL") << "\n";
    if (callEntry == NULL) {
        std::cerr << "[ERROR] Calling undeclared function '" << identifierToken->lexeme << "' at line " << yylineno << "!\n";
    }
    call_stmt->symtab_entry = callEntry; */

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
    expr->desc.var_ref->index = indexExpr;

    ENTRY* symtableEntry = NULL;

    // check if identifier is referring to the return value of the current function
    if (current_subProg_symtab != NULL) {
        if (strcmp(identifierToken->lexeme, current_subProg_symtab->base.id) == 0) {
            symtableEntry = current_subProg_symtab;

            /* std::cout << "[IDENTIFIER EXPRESSION] found reference to return value, variable lexeme = " << identifierToken->lexeme << ", adding it to symtab " << current_subProg_symtab->base.id << "\n"; */
        }
    }

    // if it is not a return statement, check the symbol table for the variable that is assigned
    if (symtableEntry == NULL) {
        ENTRY* current_parameters_symtab = current_subProg_symtab != NULL ? current_subProg_symtab->ext.prog.par_list : NULL;
        symtableEntry = getEntryFromSymtable(current_varDecl_symtab, current_parameters_symtab, identifierToken->lexeme);

        /* std::cout << "[IDENTIFIER EXPRESSION] variable lexeme = " << identifierToken->lexeme <<
            ", current varDeclSymtab = " << (current_varDecl_symtab != NULL ? current_varDecl_symtab->base.id : "NULL") << 
            ", current argsSymtab = " << (current_parameters_symtab != NULL ? current_parameters_symtab->base.id : "NULL") << 
            ", found entry = " << (symtableEntry != NULL ? symtableEntry->base.id : "NULL") << 
            "\n"; */
    }

    // print error message if no symtable entry found
    if (symtableEntry == NULL) {
        std::cerr << "[ERROR] Using undeclared variable '" << identifierToken->lexeme << "' at line " << yylineno << "!\n";
    } 

    expr->desc.var_ref->symtab_entry = symtableEntry;

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

    ENTRY* callEntry = getEntryFromGlobalScope(funcToken->lexeme);
    std::cout << "[LINKING CALLEXPR '" << funcToken->lexeme << "' to call entry of global table named " << 
        (callEntry != NULL ? callEntry->base.id : "NULL") << "\n";

    if (callEntry == NULL) {
        std::cerr << "[ERROR] Calling undeclared function '" << funcToken->lexeme << "' at line " << yylineno << "!\n";
    }

    expr->desc.func_call->symtab_entry = callEntry;

    return expr;
}

void appendExprToExprList(N_EXPR* exprList, N_EXPR* expr) {
    while (exprList->next != NULL) {
        exprList = exprList->next;
    }

    exprList->next = expr;
}

/* =========================================================== */
/* ================== Symbol table helper ==================== */
/* =========================================================== */
void addMethodToGlobalScope(ENTRY* subProgEntry) {
    ENTRY* callEntry = (ENTRY*) malloc(sizeof(ENTRY));
    callEntry->typ = ENTRY::_CALL;
    callEntry->base.id = strdup(subProgEntry->base.id);
    /* callEntry->ext.prog.symtab_entry = subProgEntry; */
    
    ENTRY* global = global_varDecl_symtab;
    while (global->next != NULL) {
        global = global->next;
    }

    global->next = callEntry;
}


ENTRY* getEntryFromSymtable(ENTRY* symtable, char* identifier) {
    while (symtable != NULL) {
        // checks if the identifier match, if they match, return this symtable entry
        if (strcmp(identifier, symtable->base.id) == 0) {
            return symtable;
        }
        // if not, continue
        symtable = symtable->next;
    }

    // return NULL if nothing found
    return NULL;
}

ENTRY* getEntryFromSymtableOrGlobal(ENTRY* symtable, char* identifier) {
    ENTRY* entryInCurrent = getEntryFromSymtable(symtable, identifier);
    return entryInCurrent != NULL ? entryInCurrent : getEntryFromSymtable(global_varDecl_symtab, identifier);
}

ENTRY* getEntryFromSymtable(ENTRY* varDeclSymtab, ENTRY* paramsSymtab, char* identifier) {
    // prioritise the paramater symbol table (not sure which of the two, params and variable declarations, actually has priority)
    ENTRY* paramsEntry = getEntryFromSymtableOrGlobal(paramsSymtab, identifier);
    return paramsEntry != NULL ? paramsEntry : getEntryFromSymtable(varDeclSymtab, identifier);
}

ENTRY* getEntryFromGlobalScope(char* identifier) {
    ENTRY* entry = global_varDecl_symtab;

    while (entry != NULL) {
        if (strcmp(identifier, entry->base.id) == 0) {
            return entry;
        }
        entry = entry->next;
    }
    return NULL;
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