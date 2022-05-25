%{
    /* Lexer for pascal source files */
    /* Author: Felix Mitterer */

    extern "C" int yylex();

    #include <iostream>
    
    #include "AST/Token.h"
    #include "AST/ast_symtab.h"

    #include "calc.tab.h"

    #define PRINT(lexem)  (std::cout << "[Token: \"" << lexem << "\"]" << std::endl)
    #define PRINTC(comment, lexem) /*(std::cout << "[" << comment << ": \"" << lexem << "\"]" << std::endl)*/

    #define PRINT_ERROR_SYMBOL(lexem) (std::cerr << "ERROR: reading symbol \"" << lexem << "\" on line " << yylineno << std::endl)
    #define PRINT_ERROR_IDENTIFIER(lexem) (std::cerr << "ERROR: not allowed identifier  \"" << lexem << "\" on line " << yylineno << " (identifiers starting with digits are not allowed)" << std::endl)

    #define ACCEPT(tokentype) do { yylval.token = new Token(tokentype, yytext, yylineno); return tokentype;} while(0)

    #define TokenType yytokentype
%}

%option yylineno

whitespace  [ \t]
newline     "\n"
digit       [0-9]

integer     {digit}+
real        {digit}+(\.{digit}+)?
string      "'"[^']*"'"

identifier  [a-zA-Z_][a-zA-Z0-9_]*
wrong_identifier [0-9][a-zA-Z0-9_]*

%%

 /* --- Comments --- */
"{"[^}]*"}" { PRINTC("Ignoring comment", yytext); }

 /* --- Keywords --- */
program     { ACCEPT(TokenType::PROGRAM); }
function    { ACCEPT(TokenType::FUNCTION); }
procedure   { ACCEPT(TokenType::PROCEDURE); }
begin       { ACCEPT(TokenType::BEGIN_); }
end         { ACCEPT(TokenType::END_); }


while       { ACCEPT(TokenType::WHILE); }
do          { ACCEPT(TokenType::DO); }

if          { ACCEPT(TokenType::IF); }
then        { ACCEPT(TokenType::THEN); }
else        { ACCEPT(TokenType::ELSE); }

var         { ACCEPT(TokenType::VAR); }
of          { ACCEPT(TokenType::OF); }

 /* --- Syntactial token --- */
","         { ACCEPT(TokenType::COMMA); }
":"         { ACCEPT(TokenType::COLON); }
";"         { ACCEPT(TokenType::SEMICOLON); }
"."         { ACCEPT(TokenType::DOT); }
".."        { ACCEPT(TokenType::RANGE_DOTS); }
"("         { ACCEPT(TokenType::BRACKETS_OPEN); }
")"         { ACCEPT(TokenType::BRACKETS_CLOSING); }
"["         { ACCEPT(TokenType::SQUARE_OPEN); }
"]"         { ACCEPT(TokenType::SQUARE_CLOSING); }

 /* --- Data type leywords --- */
integer     { ACCEPT(TokenType::INTEGER); }
real        { ACCEPT(TokenType::REAL); }
boolean     { ACCEPT(TokenType::BOOLEAN); }
array       { ACCEPT(TokenType::ARRAY); }

 /* --- Operators --- */
":="        { ACCEPT(TokenType::OP_ASSIGNMENT); }
not         { ACCEPT(TokenType::OP_NOT); }

"="         { ACCEPT(TokenType::OP_EQUALS); }
"<>"        { ACCEPT(TokenType::OP_NOT_EQUALS); }
"<"         { ACCEPT(TokenType::OP_LESS);  }
"<="        { ACCEPT(TokenType::OP_LESS_EQUAL); }
">"         { ACCEPT(TokenType::OP_GREATER);  }
">="        { ACCEPT(TokenType::OP_GREATER_EQUAL); }

 /* Arithmetic operators */
"+"         { ACCEPT(TokenType::OP_ADD); }
"-"         { ACCEPT(TokenType::OP_SUB); }
"*"         { ACCEPT(TokenType::OP_MUL); }
"/"         { ACCEPT(TokenType::OP_DIV); }
div         { ACCEPT(TokenType::OP_INTEGER_DIV); }
 /* Logic operators */
and         { ACCEPT(TokenType::OP_AND); }
or          { ACCEPT(TokenType::OP_OR); }


 /* Literals */
true                { ACCEPT(TokenType::LITERAL_TRUE); }
false               { ACCEPT(TokenType::LITERAL_FALSE); }
{integer}           { ACCEPT(TokenType::LITERAL_INTEGER); }
{real}              { ACCEPT(TokenType::LITERAL_REAL); }
{string}            { ACCEPT(TokenType::LITERAL_STRING); }


 /* Identifier */
{identifier}        { ACCEPT(TokenType::IDENTIFIER); }

{newline}           { }  /* consume new lines */ 
{whitespace}        { }  /* consume all remaining whitespaces */

 /* Catching errors */
{wrong_identifier}  { PRINT_ERROR_IDENTIFIER(yytext); } /* identifiers starting with a digit */
.                   { PRINT_ERROR_SYMBOL(yytext); } /* everything else that was not recognized yet, must be erroneous */

%%

    // "+"                 {ACCEPT(yytokentype::OP_ADD);}
    // [a-zA-Z]            {yylval.id = yytext[0]; ACCEPT(yytokentype::IDENTIFIER); }
    // [0-9]+              {yylval.num = atoi(yytext); ACCEPT(yytokentype::LITERAL_INTEGER);}
    // [ \t\n]             ;
    // [-+=;]              {ACCEPT(yytext[0]);}
    // .                   {ECHO; yyerror("unexpected character");}

    // %%

int yywrap (void) { return 1; }