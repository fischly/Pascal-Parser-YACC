#include <iostream>

#include "Token.h"
#include "ast_symtab.h"

using std::cout;

// forward declarations
void printProgram(N_PROG* program);
void printSubprogram(N_PROG* subprogram);
void printStmt(N_STMT* stmt);
void printAssignStmt(N_ASSIGN* stmt);
void printIfStmt(N_IF* stmt);
void printWhileStmt(N_WHILE* stmt);
void printCallStmt(N_CALL* stmt);
void printExpr(N_EXPR* expr);
void printConstantExpr(N_EXPR* expr);
void printVarRefExpr(N_EXPR* expr);
void printOPExpr(N_EXPR* expr);
void printFuncCallExpr(N_EXPR* expr);


/* =========================================================== */
/* ================ Program & Method printing ================ */
/* =========================================================== */
void printProgram(N_PROG* program) {
    // header
    cout << "program " << program->symtab_entry->base.id << ";\n";

    // main block statement
    printStmt(program->stmt);

    // other methods (sub programs)
    N_PROG* other = program->next;
    while (other != NULL) {
        cout << "\n\n\n";
        printSubprogram(other);
        other = other->next;
    }

    cout << ".\n";
}

void printSubprogram(N_PROG* subprogram) {
    cout << "subprogram " << subprogram->symtab_entry->base.id << ";\n";
    printStmt(subprogram->stmt);
}

/* =========================================================== */
/* =================== Statement printing ==================== */
/* =========================================================== */
void printStmt(N_STMT* stmt) {
    switch (stmt->typ)
    {
        case N_STMT::_ASSIGN:    printAssignStmt(stmt->node.assign_stmt); break;
        case N_STMT::_IF:        printIfStmt(stmt->node.if_stmt); break;
        case N_STMT::_WHILE:     printWhileStmt(stmt->node.while_stmt); break;
        case N_STMT::_PROC_CALL: printCallStmt(stmt->node.proc_call); break;

        default: cout << "ERROR: got unexpected statement type!\n"; exit(1); break;
    }

    if (stmt->next != NULL) {
        cout << ";\n";
        printStmt(stmt->next);
    }
}

void printAssignStmt(N_ASSIGN* stmt) {
    // cout << "[PRINT ASSIGN] SYMTAB ENTRY = "    << stmt->var_ref->symtab_entry << ", " << stmt->var_ref->id << "\n";
    cout << "(assign " << stmt->var_ref->symtab_entry->base.id;
    
    // optional index
    if (stmt->var_ref->index != NULL) {
        cout << "[";
        printExpr(stmt->var_ref->index);
        cout << "]";
    }
    cout << " := ";
    printExpr(stmt->rhs_expr);
    cout << ")";
}

void printIfStmt(N_IF* stmt) {
    cout << "(if ";
    printExpr(stmt->expr);
    cout << " then (\n";
    printStmt(stmt->then_part);
    cout << ")\n";

    if (stmt->else_part != NULL) {
        cout << "else (\n";
        printStmt(stmt->else_part);
        cout << ")";
    }
    cout << ")";
}

void printWhileStmt(N_WHILE* stmt) {
    cout << "(while ";
    printExpr(stmt->expr);
    cout << " do \n";
    printStmt(stmt->stmt);
    cout << ")";
}

void printCallStmt(N_CALL* stmt) {
    cout << "(call " << stmt->id;
    // print parameter list
    N_EXPR* parameter = stmt->par_list;
    while (parameter != NULL) {
        cout << " ";
        printExpr(parameter);
        parameter = parameter->next;
    }
    cout << ")";
}

/* =========================================================== */
/* ================== Expression printing ==================== */
/* =========================================================== */
void printExpr(N_EXPR* expr) {
    switch (expr->typ)
    {
        case N_EXPR::CONSTANT:  printConstantExpr(expr); break;
        case N_EXPR::VAR_REF:   printVarRefExpr(expr); break;
        case N_EXPR::OP:        printOPExpr(expr); break;
        case N_EXPR::FUNC_CALL: printFuncCallExpr(expr); break;

        default: cout << "ERROR: got unexpected expression type!\n"; exit(1); break;
    }
}

void printConstantExpr(N_EXPR* expr) {
    // just print literal value
    cout << expr->desc.constant;
}

void printVarRefExpr(N_EXPR* expr) {
    // print identifier name
    cout << expr->desc.var_ref->symtab_entry->base.id;

    // optional array index
    if (expr->desc.var_ref->index != NULL) {
        cout << "[";
        printExpr(expr->desc.var_ref->index);
        cout << "]";
    }
}

void printOPExpr(N_EXPR* expr) {
    cout << "(" << expr->desc.operation.op->lexeme << " ";
    // print left expr
    printExpr(expr->desc.operation.expr);

    // print optional right expr
    if (expr->desc.operation.expr->next != NULL) {
        cout << " ";
        printExpr(expr->desc.operation.expr->next);
    }
    cout << ")";
}

void printFuncCallExpr(N_EXPR* expr) {
    // print method name
    cout << "(call " << expr->desc.func_call->id << " ";
    // print parameter list
    N_EXPR* parameter = expr->desc.func_call->par_list;
    while (parameter != NULL) {
        printExpr(parameter);
        cout << " ";
        parameter = parameter->next;
    }
    cout << ")";
}