
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
        ss << " (defs ";

        for (const auto& declVar : declarations) {
            ss << " (" << declVar->name->lexeme << ": ";

            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(declVar->type)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(declVar->type)) {
                ss << arrayVar->typeName->lexeme << "[" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "]";
            }

            ss << ")";
        }

        ss << ")\n";
    }


public:

    /* --------------- Program ----------------- */
    void visitProgram(Program* prog) {
        ss << "(program " << prog->identifier->lexeme;

        declarations(*prog->declarations);

        // methods
        for (const auto& meth : *prog->methods) {
            meth->accept(this);
            ss << "\n\n";
        }

        ss << "(main\n";
        prog->main->accept(this);
        ss << ")";
    };


    /* --------------- Methods ----------------- */
    void visitMethod(Method* meth) {
        ss << "(method " << meth->identifier->lexeme << " (args";

        for (const auto& argVar : *meth->arguments) {
            ss << " (" << argVar->name->lexeme << ": ";

            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(argVar->type)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(argVar->type)) {
                ss << arrayVar->typeName->lexeme << "[" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "]";
            }
            
            ss << ")";
        }
        ss << ")";

        ss << " (defs";
        for (const auto& declVar : *meth->declarations) {
            ss << " (" << declVar->name->lexeme << ": ";

            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(declVar->type)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(declVar->type)) {
                ss << arrayVar->typeName->lexeme << "[" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "]";
            }

            ss << ")";
        }

        ss << ")";
        
        if (meth->returnType != NULL) {
            ss << " (returns ";
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(meth->returnType)) {
                ss << simpleVar->typeName->lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(meth->returnType)) {
                ss << arrayVar->typeName->lexeme << "[" << arrayVar->startRange->lexeme << ".." << arrayVar->stopRange->lexeme << "]";
            }
            ss << ")";
        }
        ss << "\n";

        meth->block->accept(this);
    };

    /* --------------- Statements ----------------- */
    void visitAssignment(Stmt::Assignment* stmt) {
        ss << "(assign " << stmt->identifier->lexeme;
        if (stmt->arrayIndex != NULL) {
            ss << "[";
            stmt->arrayIndex->accept(this);
            ss << "]";
        }
        ss << " := ";
        stmt->value->accept(this);
        ss << ")";
    };

    void visitCall(Stmt::Call* stmt) {
         // convert the vector to list
        std::list<Expr::Expression*> exprList;
        std::copy(stmt->arguments->begin(), stmt->arguments->end(), std::back_inserter(exprList));

        parenthesize(stmt->callee->lexeme, exprList);
    };

    void visitIf(Stmt::If* stmt) {
        ss << "(if ";
        stmt->condition->accept(this);
        ss << " (then ";
        stmt->thenBody->accept(this);
        ss << ")";

        if (stmt->elseBody != NULL) {
            ss << " (else ";
            stmt->elseBody->accept(this);
            ss << ")";
        }
        ss << ")";
    };

    void visitWhile(Stmt::While* stmt) {
        ss << "(while ";
        stmt->condition->accept(this);
        ss << " (do ";
        stmt->body->accept(this);
        ss << "))";
    };

    void visitBlock(Stmt::Block* stmt) {
        ss << "(";
        for (auto const& stmtInside : *stmt->statements) {
            stmtInside->accept(this);
        }
        ss << ")";
    };


    /* --------------- Expressions ---------------- */
    void visitBinary(Expr::Binary* expr) {
        parenthesize(expr->op->lexeme, {expr->left, expr->right});
    };

    void visitCall(Expr::Call* expr) {
        // convert the vector to list
        std::list<Expr::Expression*> exprList;
        std::copy(expr->arguments->begin(), expr->arguments->end(), std::back_inserter(exprList));

        parenthesize(expr->callee->lexeme, exprList);
    };

    void visitGrouping(Expr::Grouping* expr) {
        parenthesize("group", {expr->expression});
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
        parenthesize(expr->op->lexeme, {expr->right});
    };

    std::string getResult() {
        return ss.str();
    }
};