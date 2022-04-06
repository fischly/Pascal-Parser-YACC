
#pragma once

#include <string.h>

class Token {
public:
    int type;
    char* lexeme;
    int lineNumber;

    Token(int type, char* lexeme, int lineNumber)
        : type{type}, lineNumber{lineNumber}
    {
        this->lexeme = strdup(lexeme); // TODO: free memory
    }
};