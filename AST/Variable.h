
#pragma once

#include <vector>

#include "Statement.h"
#include "Token.h"

using Stmt::Statement;

class Variable {
public:

    class VariableType {
    public:
        virtual ~VariableType() = default;
        VariableType(Token typeName) : typeName{typeName} {}

        Token typeName;
    };

    class VariableTypeSimple : public VariableType {
    public:
        VariableTypeSimple(Token typeName) : VariableType(typeName) {}
    };

    class VariableTypeArray : public VariableType {
    public:
        VariableTypeArray(Token typeName, Token startRange, Token stopRange) 
            : VariableType(typeName), startRange{startRange}, stopRange{stopRange}
        {}        

        Token startRange;
        Token stopRange;
    };


    Variable(Token name, VariableType* type) : name{name}, type{type} {}
    ~Variable() {  }

    Token name;
    VariableType* type;
};