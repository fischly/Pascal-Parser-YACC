#include <iostream>
#include <string>
#include <sstream>
#include <list>

#include "Expression.h"
#include "Statement.h"
#include "Method.h"
#include "Program.h"

using std::cout;
using std::vector;

class ASTPrinter {
public:
    void printProgram(Program* program) {
        // header
        cout << "program " << program->identifier->lexeme << ";\n";
        // declarations
        printDeclarations(program->declarations);

        // methods
        for (const auto& meth : *program->methods) {
            printMethod(meth);
            cout << "\n\n";
        }

        // main block statement
        printStmt(program->main);

        cout << ".\n";
    }

    void printMethod(Method* method) {
        // function or procedure
        if (method->returnType != NULL) {
            cout << "function ";
        } else {
            cout << "procedure ";
        }

        // method name
        cout << method->identifier->lexeme;

        // arguments
        cout << "(";
        for (auto const& argVar : *method->arguments) {
            // argument name
            cout << argVar->name->lexeme << ": ";

            // argument type
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(argVar->type)) {
                cout << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(argVar->type)) {
                cout << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme;
            }
            
            // seperating semicolon (if not last argument)
            if (argVar != method->arguments->back()) {
                cout << "; ";
            }
        }
        cout << ")";

        // return type
        if (method->returnType != NULL) {
            cout << ": ";
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(method->returnType)) {
                cout << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(method->returnType)) {
                cout << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme;
            }
        }
        cout << ";\n";

        // declarations
        printDeclarations(method->declarations);

        // statement block
        printStmt(method->block);

        // each method ends with a ;
        cout << ";";
    }

    /* ------------------------------------------------ */
    /* ----------------- Statements ------------------- */
    /* ------------------------------------------------ */
    void printStmt(Stmt::Statement* stmt) {
        // try to type cast to find out which statement we are working with
        // and call the according function afterwards
        if (Stmt::Assignment* assignStmt = dynamic_cast<Stmt::Assignment*>(stmt)) {
            printAssignmentStmt(assignStmt);
        } else if (Stmt::Block* blockStmt = dynamic_cast<Stmt::Block*>(stmt)) {
            printBlockStmt(blockStmt);
        } else if (Stmt::Call* callStmt = dynamic_cast<Stmt::Call*>(stmt)) {
            printCallStmt(callStmt);
        } else if (Stmt::If* ifStmt = dynamic_cast<Stmt::If*>(stmt)) {
            printIfStmt(ifStmt);
        } else if (Stmt::While* whileStmt = dynamic_cast<Stmt::While*>(stmt)) {
            printWhileStmt(whileStmt);
        } 
    }

    void printAssignmentStmt(Stmt::Assignment* stmt) {
        // variable name that is assigned
        cout << stmt->identifier->lexeme;
        // variable array index (if applicable) 
        if (stmt->arrayIndex != NULL) {
            cout << "[";
            printExpr(stmt->arrayIndex);
            cout << "]";
        }
        // assignment sign
        cout << " := ";
        // the "right" side of the assignment
        printExpr(stmt->value);
    }

    
    void printBlockStmt(Stmt::Block* stmt) {
        cout << "\nbegin";
        // print each statement that is contained in this block
        for (auto const& stmtInside : *stmt->statements) {
            cout << "\n";
            printStmt(stmtInside);

            // print seperating ; if not last statement
            if (stmtInside != stmt->statements->back()) {
                cout << ";";
            }
        }
        cout << "\nend\n";
    }

    void printCallStmt(Stmt::Call* stmt) {
        // name of the called method and the opening bracket
        cout << stmt->callee->lexeme << "(";
        // all the arguments of the call
        for (auto const& exprArg : *stmt->arguments) {
            // print the expression
            printExpr(exprArg);

            // print , as seperator if not last argument
            if (exprArg != stmt->arguments->back()) {
                cout << ", ";
            }
        }
        cout << ")";
    }

    void printIfStmt(Stmt::If* stmt) {
        cout << "\nif ";
        printExpr(stmt->condition);
        cout << " then\n";
        printStmt(stmt->thenBody);

        if (stmt->elseBody != NULL) {
            cout << "\nelse ";
            printStmt(stmt->elseBody);
        }
    }

    void printWhileStmt(Stmt::While* stmt) {
        cout << "\nwhile ";
        printExpr(stmt->condition);
        cout << "\ndo\n";
        printStmt(stmt->body);
    }



    /* ------------------------------------------------ */
    /* ---------------- Expressions ------------------- */
    /* ------------------------------------------------ */
    void printExpr(Expr::Expression* expr) {
        // try to type cast to find out which expression we are working with
        // and call the according function afterwards
        if (Expr::Binary* binaryExpr = dynamic_cast<Expr::Binary*>(expr)) {
            printBinaryExpr(binaryExpr);
        } else if (Expr::Call* callExpr = dynamic_cast<Expr::Call*>(expr)) {
            printCallExpr(callExpr);
        } else if (Expr::Grouping* groupExpr = dynamic_cast<Expr::Grouping*>(expr)) {
            printGroupingExpr(groupExpr);
        } else if (Expr::Identifier* identExpr = dynamic_cast<Expr::Identifier*>(expr)) {
            printIdentifierExpr(identExpr);
        } else if (Expr::Literal* literalExpr = dynamic_cast<Expr::Literal*>(expr)) {
            printLiteralExpr(literalExpr);
        } else if (Expr::Unary* unaryExpr = dynamic_cast<Expr::Unary*>(expr)) {
            printUnaryExpr(unaryExpr);
        }
    }

    void printBinaryExpr(Expr::Binary* expr) {
        // left side of the binary expression
        printExpr(expr->left);
        // operator
        cout << " " << expr->op->lexeme << " ";
        // right side of the binary expression
        printExpr(expr->right);

    }

    void printCallExpr(Expr::Call* expr) {
        // called method name
        cout << expr->callee->lexeme << "(";
        // all arguments
        for (auto const& exprArg : *expr->arguments) {
            printExpr(exprArg);

            // seperating ,
            if (exprArg != expr->arguments->back()) {
                cout << ", ";
            }
        }
        cout << ")";
    }

    void printGroupingExpr(Expr::Grouping* expr) {
        cout << "(";
        // the contained expression
        printExpr(expr->expression);
        cout << ")";
    }

    void printIdentifierExpr(Expr::Identifier* expr) {
        // the name of the identifier
        cout << expr->token->lexeme;

        // optionally, the array index
        if (expr->arrayIndexExpression != NULL) {
            cout << "[";
            printExpr(expr->arrayIndexExpression);
            cout << "]";
        }
    }

    void printLiteralExpr(Expr::Literal* expr) {
        // just the literal value
        cout << expr->token->lexeme;
    }

    void printUnaryExpr(Expr::Unary* expr) {
        // operator and the right side expression
        cout << expr->op->lexeme;
        printExpr(expr->right);
    }




    void printDeclarations(vector<Variable*>* declarations) {
        if (declarations->size() > 0) {
            cout << "  var ";

            for (const auto& declVar : *declarations) {
                cout << "      " << declVar->name->lexeme << ": ";

                if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(declVar->type)) {
                    cout << simpleVar->typeName->lexeme << ";\n";
                } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(declVar->type)) {
                    cout << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme << ";\n";
                }
            }
        }

        cout << "\n";
    }
};