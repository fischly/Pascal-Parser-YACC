%{
#include "y.tab.h"

int position = 0;

%}

%option yylineno

newline     [\n]+
tab         [\t]+
whitespace  [ ]+
comment     \{.*\}
number      [0-9]+(\.[0-9]+)?
identifier  [_a-zA-Z][_a-zA-Z0-9]*

%%


{newline}       { position = 0;}
{tab}           { position += yyleng;}
{whitespace}    { position += yyleng;}
{comment}       { printf("(%02d | %02d - %02d) LEX::comment::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng;}

"program"       { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return PROGRAM;}
"var"           { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return VAR; }
"array"         { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return ARRAY; }
"of"            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return OF; }
"function"      { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return FUNCTION; }
"procedure"     { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return PROCEDURE; }
"begin"         { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BBEGIN; }
"end"           { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return END; }
"while"         { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return WHILE; }
"do"            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return DO; }
"then"          { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return THEN; }
"if"            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return IF; }
"else"          { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return ELSE; }

"integer"       { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return INTEGER; }
"real"          { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return REAL; }
"boolean"       { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BOOLEAN; }
"true"          { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return TRUE; }
"false"         { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return FALSE; }

"="             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return EQUAL; }
"<"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return LESS; }
">"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return GREATER; }
">="            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return EQUAL_GREATER; }
"<="            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return EQUAL_LESS; }
"<>"            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return NOT_EQUAL; }
":="            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return ASSIGN; }

"."             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return POINT; }
","             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return COMMA; }
";"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return SEMICOLON; }
":"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return DOUBLE_POINT; }

"+"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return PLUS; }
"-"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return MINUS; }
"*"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return MULT; }
"/"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return DIV_MATH; }
"div"           { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return DIV_TEXT; }
"and"           { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return AND; }
"mod"           { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return MOD; }
"or"            { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return OR; }

"("             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_ROUND_OPEN; }
")"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_ROUND_CLOSE; }
"["             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_SQUARE_OPEN; }
"]"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_SQUARE_CLOSE; }
"{"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_CURLY_OPEN; }
"}"             { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return BRACKET_CURLY_CLOSE; }

{number}        { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return NUMBER;}
{identifier}    { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return IDENTIFIER; }

.               { printf("(%02d | %02d - %02d) LEX::[%s]\n", yylineno, position, position+yyleng, yytext); position += yyleng; return ERROR;}

%%

int yywrap(void){
    return 1;
}