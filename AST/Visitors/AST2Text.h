
#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <list>

#include "../Expression.h"
#include "../Statement.h"
#include "../Method.h"
#include "../Program.h"

/**
 * Transforms an AST to a textual representation (similar to LISP), that allows seeing the precendence.
 */
class AST2Text : public Expr::Visitor, public Stmt::Visitor, public Method::Visitor, public Program::Visitor {
private:
    std::stringstream ss; // holds the result

    // helper function to paranthesize expressions (LISP like)
    void parenthesize(std::string name, const std::list<Expr::Expression*>& expressions) {
        ss << "(" << name;

        for (auto const& exp : expressions) {
            ss << " ";
            exp->accept(this);
        }

        ss << ")";
    }

    // helper function to print declarations (for <program> and <function>/<procedure>)
    void declarations(std::vector<Variable*>& declarations) {
        if (declarations.size() > 0) {
             ss << "  var ";

            for (const auto& declVar : declarations) {
                ss << "      " << declVar->name->lexeme << ": ";

                if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(declVar->type)) {
                    ss << simpleVar->typeName->lexeme << ";\n";
                } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(declVar->type)) {
                    ss << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme << ";\n";
                }
            }
        }

        ss << "\n";
    }


public:

    /* --------------- Program ----------------- */
    void visitProgram(Program* prog) {
        ss << "program " << prog->identifier->lexeme << ";\n";

        declarations(*prog->declarations);

        // methods
        for (const auto& meth : *prog->methods) {
            meth->accept(this);
            ss << "\n\n";
        }

        prog->main->accept(this);

        ss << ".\n";
    };


    /* --------------- Methods ----------------- */
    void visitMethod(Method* meth) {
        if (meth->returnType != NULL) {
            ss << "function ";
        } else {
            ss << "procedure ";
        }
        ss << meth->identifier->lexeme;

        // arguments
        ss << "(";
        for (auto const& argVar : *meth->arguments) {
            ss << argVar->name->lexeme << ": ";

            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(argVar->type)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(argVar->type)) {
                ss << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme;
            }
            
            if (argVar != meth->arguments->back()) {
                ss << "; ";
            }
            
        }
        ss << ")";
        
        // return type
        if (meth->returnType != NULL) {
            ss << ": ";
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(meth->returnType)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(meth->returnType)) {
                ss << "array [" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "] of " << arrayVar->typeName->lexeme;
            }
        }
        ss << ";\n";

        // declarations
        declarations(*meth->declarations);

        meth->block->accept(this);
        ss << ";"; // each method ends with a ;
    };

    /* --------------- Statements ----------------- */
    void visitAssignment(Stmt::Assignment* stmt) {
        ss << stmt->identifier->lexeme; 
        if (stmt->arrayIndex != NULL) {
            ss << "[";
            stmt->arrayIndex->accept(this);
            ss << "]";
        }
        ss << " := ";
        stmt->value->accept(this);
    };

    void visitCall(Stmt::Call* stmt) {
        ss << stmt->callee->lexeme << "(";
        for (auto const& exprArg : *stmt->arguments) {
            exprArg->accept(this);

            if (exprArg != stmt->arguments->back()) {
                ss << ", ";
            }
        }
        ss << ")";
    };

    void visitIf(Stmt::If* stmt) {
        ss << "\nif ";
        stmt->condition->accept(this);
        ss << " then\n";
        stmt->thenBody->accept(this);

        if (stmt->elseBody != NULL) {
            ss << "\nelse ";
            stmt->elseBody->accept(this);
        }
    };

    void visitWhile(Stmt::While* stmt) {
        ss << "\nwhile ";
        stmt->condition->accept(this);
        ss << "\ndo\n";
        stmt->body->accept(this);
    };

    void visitBlock(Stmt::Block* stmt) {
        ss << "\nbegin";
        for (auto const& stmtInside : *stmt->statements) {
            ss << "\n";
            stmtInside->accept(this);

            if (stmtInside != stmt->statements->back()) {
                ss << ";";
            }
        }
        ss << "\nend\n";
    };


    /* --------------- Expressions ---------------- */
    void visitBinary(Expr::Binary* expr) {
        expr->left->accept(this);
        ss << " " << expr->op->lexeme << " ";
        expr->right->accept(this);
    };

    void visitCall(Expr::Call* expr) {
        ss << expr->callee->lexeme << "(";
        for (auto const& exprArg : *expr->arguments) {
            exprArg->accept(this);

            if (exprArg != expr->arguments->back()) {
                ss << ", ";
            }
        }
        ss << ")";
    };

    void visitGrouping(Expr::Grouping* expr) {
        ss << "(";
        expr->expression->accept(this);
        ss << ")";
    };

    void visitIdentifier(Expr::Identifier* expr) {
        ss << expr->token->lexeme;

        if (expr->arrayIndexExpression != NULL) {
            ss << "[";
            expr->arrayIndexExpression->accept(this);
            ss << "]";
        }
    };

    void visitLiteral(Expr::Literal* expr) {
        ss << expr->token->lexeme;
    };

    void visitUnary(Expr::Unary* expr) {
        ss << expr->op->lexeme;
        expr->right->accept(this);
    };

    std::string getResult() {
        return ss.str();
    }
};