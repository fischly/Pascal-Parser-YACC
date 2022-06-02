#pragma once

#include <iostream>

#include "Token.h"
#include "ast_symtab.h"
#include "../calc.tab.h"

using std::cout;

// counter to uniquely identify if/while loops
unsigned long counter = 0; 

// forward declarations
void assembleProgram(N_PROG* program);
void assembleVarDec(ENTRY* varDec);
void assembleSubprogram(N_PROG* subprogram);
void assembleStmt(N_STMT* stmt);
void assembleAssignStmt(N_ASSIGN* stmt);
void assembleIfStmt(N_IF* stmt);
void assembleWhileStmt(N_WHILE* stmt);
void assembleCallStmt(N_CALL* stmt);
void assembleExpr(N_EXPR* expr);
void assembleConstantExpr(N_EXPR* expr);
void assembleVarRefExpr(N_EXPR* expr);
void assembleOPExpr(N_EXPR* expr);
void assembleFuncCallExpr(N_EXPR* expr);

void pushToStack(const char* reg = "$t0");
void popFromStack(const char* reg = "$t0");


/* =========================================================== */
/* ================ Program & Method printing ================ */
/* =========================================================== */
void assembleProgram(N_PROG* program) {
    // header

    assembleVarDec(program->symtab_entry->next);


    cout << ".text\n"; // << program->symtab_entry->base.id << ";\n";
    cout << ".globl main     # " << program->symtab_entry->base.id << "\n\n";
    cout << "main:\n\n";

    // main block statement
    assembleStmt(program->stmt);

    // other methods (sub programs)
    /*N_PROG* other = program->next;
    while (other != NULL) {
        cout << "\n\n\n";
        assembleSubprogram(other);
        other = other->next;
    }*/
}

void assembleVarDec(ENTRY* varDec) {
    // create .data segment
    cout << ".data\n";
    
    while (varDec != NULL) {
        cout << "  " << varDec->base.id << ": ";
        switch (varDec->data_type) {
            case DATA_TYPE::_BOOL: cout << ".byte 0"; break; /* using .byte for booleans, defining 0 as false and everything else as true */
            case DATA_TYPE::_INT:  cout << ".word 0"; break;
            case DATA_TYPE::_REAL: cout << ".float 0"; break;
            default: std::cerr << "[ERROR] tried to assemble a variable declaration that was of data type VOID\n"; break;
        }
        cout << "\n";

        varDec = varDec->next;
    }

    cout << "\n\n";
}

void assembleSubprogram(N_PROG* subprogram) {
    cout << "subprogram " << subprogram->symtab_entry->base.id << ";\n";
    assembleStmt(subprogram->stmt);
}

/* =========================================================== */
/* =================== Statement printing ==================== */
/* =========================================================== */
void assembleStmt(N_STMT* stmt) {
    switch (stmt->typ)
    {
        case N_STMT::_ASSIGN:    assembleAssignStmt(stmt->node.assign_stmt); break;
        case N_STMT::_IF:        assembleIfStmt(stmt->node.if_stmt); break;
        case N_STMT::_WHILE:     assembleWhileStmt(stmt->node.while_stmt); break;
        case N_STMT::_PROC_CALL: assembleCallStmt(stmt->node.proc_call); break;

        default: cout << "ERROR: got unexpected statement type!\n"; exit(1); break;
    }

    if (stmt->next != NULL) {
        cout << "\n";
        assembleStmt(stmt->next);
    }
}

void assembleAssignStmt(N_ASSIGN* stmt) {
    // evaluate expression
    assembleExpr(stmt->rhs_expr);

    // store into memory
    cout << "\n";
    cout << "# --- [STMT ASSIGN " << stmt->var_ref->symtab_entry->base.id << "] --- #\n";
    cout << "lw  \t$t0, 0($sp) # pop into $t0\n";
    cout << "addi\t$sp, $sp, 4 # pop\n";
    cout << "sw  \t$t0, " << stmt->var_ref->symtab_entry->base.id << "    # " << stmt->var_ref->symtab_entry->base.id << " = $t0\n";
    cout << "\n";


    // // cout << "[PRINT ASSIGN] SYMTAB ENTRY = "    << stmt->var_ref->symtab_entry << ", " << stmt->var_ref->id << "\n";
    // // TODO: remove when all variables get assigned
    // if (stmt->var_ref->symtab_entry != NULL) {
    //     cout << "(assign " << stmt->var_ref->symtab_entry->base.id;
    // } else {
    //     cout << "(assign " << stmt->var_ref->id;
    // }
    // // optional index
    // if (stmt->var_ref->index != NULL) {
    //     cout << "[";
    //     assembleExpr(stmt->var_ref->index);
    //     cout << "]";
    // }
    // cout << " := ";
    // assembleExpr(stmt->rhs_expr);
    // cout << ")";
}

void assembleIfStmt(N_IF* stmt) {
    unsigned long id = counter++;

    cout << "# --- [STMT IF" << id << "] --- #\n";
    cout << "# --- [IF COND] --- #\n";
    // assemble condition expression
    assembleExpr(stmt->expr);

    // pop condition epxression result
    popFromStack();

    // branch to else if the condition is 0 (= false)
    cout << "# --- [IF BRANCH] --- #\n";
    cout << "beq \t$t0, $0, Else" << id << "\n";
    
    // then part
    cout << "# --- [IF THEN] --- #\n";
    assembleStmt(stmt->then_part);

    // jump to end of if (so the else part won't get executed)
    cout << "j   \t" << "Endif" << id << "\n"; 

    // else part
    cout << "# --- [IF ELSE] --- #\n";
    cout << "\nElse" << id << ":\n";
    
    if (stmt->else_part != NULL) {
        assembleStmt(stmt->else_part);
    }
    // endif mark
    cout << "Endif" << id << ":\n";
    cout << "# --- [IF END] --- #\n";
}

void assembleWhileStmt(N_WHILE* stmt) {
    unsigned long id = counter++;
    cout << "# --- [STMT WHILE" << id << "] --- #\n";

    // loop label
    cout << "Loop" << id << ":\n";
   
    cout << "# --- [WHILE COND] --- #\n";
    assembleExpr(stmt->expr);

    popFromStack();

    cout << "# --- [WHILE BRANCH] --- #\n";
    cout << "beq \t$t0, $0, LoopExit" << id << "\n";


    cout << "# --- [WHILE BODY] --- #\n";
    assembleStmt(stmt->stmt);
    cout << "j   \t" << "Loop" << id << "\n";

    cout << "LoopExit" << id << ":\n\n";
}

void assembleCallStmt(N_CALL* stmt) {
    std::cerr << "[assembleCallStmt] NOT IMPLEMENTED YET\n";

    // cout << "(call " << stmt->id;
    // // print parameter list
    // N_EXPR* parameter = stmt->par_list;
    // while (parameter != NULL) {
    //     cout << " ";
    //     assembleExpr(parameter);
    //     parameter = parameter->next;
    // }
    // cout << ")";
}

/* =========================================================== */
/* ================== Expression printing ==================== */
/* =========================================================== */
void assembleExpr(N_EXPR* expr) {
    switch (expr->typ)
    {
        case N_EXPR::CONSTANT:  assembleConstantExpr(expr); break;
        case N_EXPR::VAR_REF:   assembleVarRefExpr(expr); break;
        case N_EXPR::OP:        assembleOPExpr(expr); break;
        case N_EXPR::FUNC_CALL: assembleFuncCallExpr(expr); break;

        default: cout << "ERROR: got unexpected expression type!\n"; exit(1); break;
    }
}

void assembleConstantExpr(N_EXPR* expr) {
    // parse to int only for now
    const int constVal = std::stoi(expr->desc.constant);

    cout << "\n";
    cout << "# --- [EXPR CONSTANT " << constVal << "] --- #\n";
    cout << "addi \t$t0, $0, " << constVal << "\n";

    pushToStack();
}

void assembleVarRefExpr(N_EXPR* expr) {
    cout << "\n";
    cout << "# --- [EXPR VARREF " << expr->desc.symtab_entry->base.id << "] --- #\n";
    cout << "lw $t0, " << expr->desc.symtab_entry->base.id << "\n";
    
    pushToStack();

    // // print identifier name
    // cout << expr->desc.var_ref->id;

    // // optional array index
    // if (expr->desc.var_ref->index != NULL) {
    //     cout << "[";
    //     assembleExpr(expr->desc.var_ref->index);
    //     cout << "]";
    // }
}

void assembleBinaryOP(const char* op) {
    cout << op << " \t$t0, $t1, $t0\n";
    pushToStack();
}
void assembleOPExpr(N_EXPR* expr) {
    if (expr->desc.operation.expr->next == NULL) {
        // unary operator

        // assemble the expression
        assembleExpr(expr->desc.operation.expr);

        switch (expr->desc.operation.op->type) {
            /* unary minus */
            case yytokentype::OP_SUB: 
                cout << "# --- [EXPR UNARY MINUS] --- #\n";
                popFromStack("$t0");
                cout << "subu\t$t0, $0, $t0\n";
                pushToStack();
                break;
            /* unary logical not */
            case yytokentype::OP_NOT: /* TODO */ break;
        }

    } else {
        // binary operator

        // assemble both rhs and lhs expressions
        assembleExpr(expr->desc.operation.expr);
        assembleExpr(expr->desc.operation.expr->next);

        // pop the results from the stack
        popFromStack("$t0");
        popFromStack("$t1");

        switch (expr->desc.operation.op->type) {
            /* addition */
            case yytokentype::OP_ADD: cout << "# --- [EXPR ADDITION] --- #\n"; assembleBinaryOP("addu"); break;
            /* subtraction */
            case yytokentype::OP_SUB: cout << "# --- [EXPR SUBTRACTION] --- #\n"; assembleBinaryOP("subu"); break;
            /* multiplication */
            case yytokentype::OP_MUL: cout << "# --- [EXPR MULTIPLICATION] --- #\n"; assembleBinaryOP("mulu"); break;
            /* divisions */
            case yytokentype::OP_DIV:
            case yytokentype::OP_INTEGER_DIV: 
                cout << "# --- [EXPR DIVISION] --- #\n";
                cout << "divu\t$t1, $t0\n";
                cout << "mflo\t$t0\n";
                pushToStack();
                break;


            /* logical operations */
            case yytokentype::OP_EQUALS:        cout << "# --- [EXPR EQUALS] --- #\n";        assembleBinaryOP("seq"); break;
            case yytokentype::OP_NOT_EQUALS:    cout << "# --- [EXPR NOT EQUALS] --- #\n";    assembleBinaryOP("sne"); break;
            case yytokentype::OP_LESS:          cout << "# --- [EXPR LESS] --- #\n";          assembleBinaryOP("slt"); break;
            case yytokentype::OP_LESS_EQUAL:    cout << "# --- [EXPR LESS EQUAL] --- #\n";    assembleBinaryOP("sle"); break;
            case yytokentype::OP_GREATER:       cout << "# --- [EXPR GREATER] --- #\n";       assembleBinaryOP("sgt"); break;
            case yytokentype::OP_GREATER_EQUAL: cout << "# --- [EXPR GREATER EQUAL] --- #\n"; assembleBinaryOP("sge"); break;
        }
    }
}

void assembleFuncCallExpr(N_EXPR* expr) {
    std::cerr << "[assembleFuncCallExpr] NOT IMPLEMENTED YET\n";
    // // print method name
    // cout << "(call " << expr->desc.func_call->id << " ";
    // // print parameter list
    // N_EXPR* parameter = expr->desc.func_call->par_list;
    // while (parameter != NULL) {
    //     assembleExpr(parameter);
    //     cout << " ";
    //     parameter = parameter->next;
    // }
    // cout << ")";
}


/* =========================================================== */
/* ===================== Stack helper  ======================= */
/* =========================================================== */
void pushToStack(const char* reg) {
    cout << "# push " << reg << " to stack\n";
    cout << "addi\t$sp, $sp, -4\n";
    cout << "sw  \t" << reg << ", 0($sp)\n";
}

void popFromStack(const char* reg) {
    cout << "# pop from stack to " << reg << "\n";
    cout << "lw  \t" << reg << ", 0($sp)\n";
    cout << "addi\t$sp, $sp, 4\n";
}

