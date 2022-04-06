
#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <list>
#include <map>

#include "../Expression.h"
#include "../Statement.h"
#include "../Method.h"
#include "../Program.h"

/**
 * Transforms an AST to a textual representation (similar to LISP), that allows seeing the precendence.
 */
class AST2Dot : public Expr::Visitor, public Stmt::Visitor, public Method::Visitor, public Program::Visitor {
private:
    std::stringstream ss; // holds the result
    std::map<void*, std::string> nodeNames; // holds unique names for each node
    unsigned int counter = 0;

    /**
     * Gets unique name of a node.
     */
    std::string getNodeName(void* expOrStmt) {
        if (nodeNames.count(expOrStmt)) {
            return nodeNames.find(expOrStmt)->second;
        }

        nodeNames.insert(std::pair<void*, std::string>(expOrStmt, std::to_string(counter++)));
        return nodeNames.find(expOrStmt)->second;
    }


public:

    /* --------------- Program ----------------- */
    void visitProgram(Program* prog) {
        ss << "digraph G {\n\n";

        // declarations(prog->declarations);

        // methods
        for (const auto& meth : prog->methods) {
            meth->accept(this);
        }

        prog->main->accept(this);

        ss << "}\n";
    };


    /* --------------- Methods ----------------- */
    void visitMethod(Method* meth) {
        auto methNodeName = getNodeName(meth);

        ss << "subgraph cluster" << getNodeName(meth) << "{\n";
        ss << "label = \"" << meth->identifier.lexeme << "(";

        for (const auto& argVar : meth->arguments) {
            ss << argVar->name.lexeme << ": ";
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(argVar->type)) {
                ss << simpleVar->typeName.lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(argVar->type)) {
                ss << arrayVar->typeName.lexeme << "[" << arrayVar->startRange.lexeme << ".." << arrayVar->stopRange.lexeme << "]";
            }
            ss << ", ";
        }
        ss << ")";

        if (meth->returnType != NULL) {
            ss << ": ";
            if (Variable::VariableTypeSimple* simpleVar = dynamic_cast<Variable::VariableTypeSimple*>(meth->returnType)) {
                ss << simpleVar->typeName.lexeme;
            } else if (Variable::VariableTypeArray* arrayVar = dynamic_cast<Variable::VariableTypeArray*>(meth->returnType)) {
                ss << arrayVar->typeName.lexeme << "[" << arrayVar->startRange.lexeme << ".." << arrayVar->stopRange.lexeme << "]";
            }
        }
        
        ss << "\";\n";

        meth->block->accept(this);

        ss << "}\n\n";
    };

    /* --------------- Statements ----------------- */
    void visitAssignment(Stmt::Assignment* stmt) {
        auto stmtNodeName = getNodeName(stmt);
        auto valueNodeName = getNodeName(stmt->value);

        ss << "\n";
        ss << stmtNodeName << " [label = \"" << stmt->identifier.lexeme << " = \"];\n";

        // array index expression
        if (stmt->arrayIndex != NULL) {
            auto arrayIndexNodeName = getNodeName(stmt->arrayIndex);

            ss << stmtNodeName << " -> " << arrayIndexNodeName  << ";\n";

            stmt->arrayIndex->accept(this);
        }

        ss << stmtNodeName << " -> " << valueNodeName << ";\n\n";

        stmt->value->accept(this);
    };

    void visitCall(Stmt::Call* stmt) {
        auto stmtNodeName = getNodeName(stmt);
        
        ss << "\n";
        ss << stmtNodeName << " [label = \"call " << stmt->callee.lexeme << "\"];\n";
        
        for (const auto& argExpr : stmt->arguments) {
            auto argExprNodeName = getNodeName(argExpr);
            ss << stmtNodeName << " -> " << argExprNodeName << ";\n";
            argExpr->accept(this);
        }
    };

    void visitIf(Stmt::If* stmt) {
        auto stmtNodeName = getNodeName(stmt);
        auto conditionNodeName = getNodeName(stmt->condition);
        auto thenNodeName = getNodeName(stmt->thenBody);


        ss << "\n";
        ss << stmtNodeName << " [label = \"if\", fillcolor=lightpink, style=filled];\n";
        ss << stmtNodeName << " -> " << conditionNodeName << ";\n";
        ss << stmtNodeName << " -> " << thenNodeName << ";\n";

        stmt->condition->accept(this);
        stmt->thenBody->accept(this);

        if (stmt->elseBody != NULL) {
            auto elseNodeName = getNodeName(stmt->elseBody);
            ss << stmtNodeName << " -> " << elseNodeName << ";\n";

            stmt->elseBody->accept(this);
        }
    };

    void visitWhile(Stmt::While* stmt) {
        auto stmtNodeName = getNodeName(stmt);
        auto conditionNodeName = getNodeName(stmt->condition);
        auto bodyNodeName = getNodeName(stmt->body);

        ss << "\n";
        ss << stmtNodeName << " [label = \"while\", fillcolor=lightpink, style=filled];\n";
        ss << stmtNodeName << " -> " << conditionNodeName << ";\n";
        ss << stmtNodeName << " -> " << bodyNodeName << ";\n";
         
        stmt->condition->accept(this);
        stmt->body->accept(this);
    };

    void visitBlock(Stmt::Block* stmt) {
        auto blockNodeName = getNodeName(stmt);

        ss << "\n" << blockNodeName << " [label = \"block\"];\n";

        for (auto const& stmtInside : stmt->statements) {
            auto stmtInsideNodeName = getNodeName(stmtInside);

            ss << blockNodeName << " -> " << stmtInsideNodeName << ";\n";
            stmtInside->accept(this);
        }

    };


    /* --------------- Expressions ---------------- */
    void visitBinary(Expr::Binary* expr) {
        auto exprNodeName = getNodeName(expr);
        auto leftNodeName = getNodeName(expr->left);
        auto rightNodeName = getNodeName(expr->right);

        ss << "\n";
        ss << exprNodeName << " [label = \"" << expr->op.lexeme << "\", fillcolor=gray, style=filled];\n";
        ss << exprNodeName << " -> " << leftNodeName << ";\n";
        ss << exprNodeName << " -> " << rightNodeName << ";\n\n";

        expr->left->accept(this);
        expr->right->accept(this);
    };

    void visitCall(Expr::Call* expr) {
        auto exprNodeName = getNodeName(expr);
        
        ss << "\n";
        ss << exprNodeName << " [label = \"call " << expr->callee.lexeme << "\"];\n";
        
        for (const auto& argExpr : expr->arguments) {
            auto argExprNodeName = getNodeName(argExpr);
            ss << exprNodeName << " -> " << argExprNodeName << ";\n";
            argExpr->accept(this);
        }
    };

    void visitGrouping(Expr::Grouping* expr) {
        auto exprNodeName = getNodeName(expr);
        auto innerNodeName = getNodeName(expr->expression);

        ss << "\n";
        ss << exprNodeName << " [label = \"( )\", fillcolor=gray, style=filled];\n";
        ss << exprNodeName << " -> " << innerNodeName << ";\n";

        expr->expression->accept(this); 
    };

    void visitIdentifier(Expr::Identifier* expr) {
        auto exprNodeName = getNodeName(expr);
        
        ss << "\n" << exprNodeName << " [label = \"" << expr->token.lexeme << "\", fillcolor=darkseagreen1, style=filled];\n";
    };

    void visitLiteral(Expr::Literal* expr) {
        auto exprNodeName = getNodeName(expr);

        ss << "\n" << exprNodeName << " [label = \"" << expr->token.lexeme << "\", fillcolor=lightblue, style=filled];\n";
    };

    void visitUnary(Expr::Unary* expr) {
        auto exprNodeName = getNodeName(expr);
        auto rightNodeName = getNodeName(expr->right);

        ss << "\n";
        ss << exprNodeName << " [label = \"" << expr->op.lexeme << "\", fillcolor=gray, style=filled];\n";
        ss << exprNodeName << " -> " << rightNodeName << ";\n\n";

        expr->right->accept(this);
    };

    std::string getResult() {
        return ss.str();
    }
};