
#pragma once

#include <vector>

#include "Expression.h"
#include "Token.h"

using Expr::Expression;

namespace Stmt {
    /* forward declarations */
    class Assignment; class Call; class If; class While; class Block;

     /* statement visitor */
    class Visitor {
    public:
        virtual ~Visitor() = default;

        virtual void visitAssignment(Assignment* stmt) {};
        virtual void visitCall(Call* stmt) {};
        virtual void visitIf(If* stmt) {};
        virtual void visitWhile(While* stmt) {};
        virtual void visitBlock(Block* stmt) {};
    };


    /* Base class */
    class Statement {
    public:
        virtual ~Statement() = default;
        virtual void accept(Visitor* visitor) = 0;
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

        void accept(Visitor* visitor) { visitor->visitAssignment(this); }
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

        void accept(Visitor* visitor) { visitor->visitCall(this); }
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

        void accept(Visitor* visitor) { visitor->visitIf(this); }

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

        void accept(Visitor* visitor) { visitor->visitWhile(this); }
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

        void accept(Visitor* visitor) { visitor->visitBlock(this); }
    };

};

