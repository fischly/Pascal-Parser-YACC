
#pragma once

#include <vector>

#include "Statement.h"
#include "Variable.h"


class Method {
public:
    /* method visitor */
    class Visitor {
    public:
        virtual ~Visitor() = default;

        virtual void visitMethod(Method* method) {};
    };


    Method(Token* identifier,
           std::vector<Variable*>* arguments, 
           std::vector<Variable*>* declarations, 
           Stmt::Block* block,
           Variable::VariableType* returnType)
        : identifier{identifier}, arguments{arguments}, declarations{declarations}, block{block}, returnType{returnType}
    {}
    ~Method() {
        for (auto& arg : *arguments) {
            delete arg;
        }

        for (auto& decl : *declarations) {
            delete decl;
        }

        delete arguments;
        delete declarations;

        delete block;
        delete returnType;
    }

    Token* identifier;
    std::vector<Variable*>* arguments;
    std::vector<Variable*>* declarations;
    Stmt::Block* block;
    Variable::VariableType* returnType;

    void accept(Visitor* visitor) { visitor->visitMethod(this); }
};