
#pragma once

#include <vector>

#include "Token.h"

namespace Expr {
    /* Base class for all expressions */
    class Expression {
    public:
        virtual ~Expression() = default;
    };
    

    /* different types of expressions */
    class Binary : public Expression {
    public:
        Binary(Expression* left, Token* op, Expression* right) 
            : left{left}, op{op}, right{right}
        {}
        ~Binary() {
            delete left;
            delete right;
        }

        Expression* left;
        Token* op;
        Expression* right;
    };

    class Call : public Expression {
    public:
        Call(Token* callee, std::vector<Expression*>* arguments)
            : callee{callee}, arguments{arguments}
        {}
        ~Call() {
            for (auto& arg : *arguments) {
                delete arg;
            }
        }

        Token* callee;
        std::vector<Expression*>* arguments;
    };

    class Grouping : public Expression {
    public:
        Grouping(Expression* expression) : expression{expression} {}
        ~Grouping() { delete expression; }
        
        Expression* expression;
    };

    class Identifier : public Expression {
    public:
        Identifier(Token* token, Expression* arrayIndexExpression) 
            : token{token}, arrayIndexExpression{arrayIndexExpression} {}
        ~Identifier() { delete arrayIndexExpression; }

        Token* token;
        Expression* arrayIndexExpression;
    };

    class Literal : public Expression {
    public:
        Literal(Token* token) : token{token} {}
        ~Literal() { }
        
        Token* token;
    };

    class Unary : public Expression {
    public:
        Unary(Token* op, Expression* right) : op{op}, right{right} {}
        ~Unary() { delete right; }

        Token* op;
        Expression* right;
    };

}

