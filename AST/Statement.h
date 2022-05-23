
#pragma once

#include <vector>

#include "Expression.h"
#include "Token.h"

using Expr::Expression;

namespace Stmt {
    /* Base class */
    class Statement {
    public:
        virtual ~Statement() = default;
    };

    /* different types of statements */
    class Assignment : public Statement {
    public:
        Assignment(Token* identifier, Expression* arrayIndex, Expression* value) 
            : identifier{identifier}, arrayIndex{arrayIndex}, value{value}
        {}
        ~Assignment() {
            delete arrayIndex;
            delete value;
        }

        Token* identifier;
        Expression* arrayIndex;
        Expression* value;
    };

    class Call : public Statement {
    public:
        Call(Token* callee, std::vector<Expression*>* arguments)
            : callee{callee}, arguments{arguments}
        {}
        ~Call() {
            for (auto& arg : *arguments) {
                delete arg;
            }
            delete arguments;
        }

        Token* callee;
        std::vector<Expression*>* arguments;
    };

    class If : public Statement {
    public:
        If(Expression* condition, Statement* thenBody, Statement* elseBody)
            : condition{condition}, thenBody{thenBody}, elseBody{elseBody}
        {}
        ~If() {
            delete condition;
            delete thenBody;
            delete elseBody;
        }

        Expression* condition;
        Statement* thenBody;
        Statement* elseBody;
    };

    class While : public Statement {
    public:
        While(Expression* condition, Statement* body)
            : condition{condition}, body{body}
        {}
        ~While() {
            delete condition;
            delete body;
        }

        Expression* condition;
        Statement* body;
    };

    class Block : public Statement {
    public:
        Block(std::vector<Statement*>* statements)
            : statements{statements}
        {}
        ~Block() {
            for (auto& stmt : *statements) {
                delete stmt;
            }
            delete statements;
        }

        std::vector<Statement*>* statements;
    };

};

