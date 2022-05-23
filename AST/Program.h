
#pragma once

#include <vector>

#include "Statement.h"
#include "Variable.h"
#include "Method.h"

class Program {
public:
    Program(Token* identifier, std::vector<Variable*>* declarations, std::vector<Method*>* methods, Stmt::Block* main)
        : identifier{identifier}, declarations{declarations}, methods{methods}, main{main}
    {}

    ~Program() {
        for (auto& decl : *declarations) {
            delete decl;
        }

        for (auto& meth : *methods) {
            delete meth;
        }

        delete declarations;
        delete methods;
        delete main;
    }

    Token* identifier;
    std::vector<Variable*>* declarations;
    std::vector<Method*>* methods;
    Stmt::Block* main;
};