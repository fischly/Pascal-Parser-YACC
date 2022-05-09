%{
#include "parse.tab.h"

int position = 0;

%}

%option yylineno

newline     [\n]+
tab         [\t]+
whitespace  [ ]+
comment     \{.*\}
num         [0-9]+(\.[0-9]+)?
ident       [_a-zA-Z][_a-zA-Z0-9]*

%%


{newline}       { position = 0;}
{tab}           { position += yyleng;}
{whitespace}    { position += yyleng;}
{comment}       {  position += yyleng;}

"program"       {  position += yyleng; return PROGRAM;}
"var"           {  position += yyleng; return VAR; }
"array"         {  position += yyleng; return ARRAY; }
"of"            {  position += yyleng; return OF; }
"function"      {  position += yyleng; return FUNCTION; }
"procedure"     {  position += yyleng; return PROCEDURE; }
"begin"         {  position += yyleng; return _BEGIN; }
"end"           {  position += yyleng; return _END; }
"while"         {  position += yyleng; return WHILE; }
"do"            {  position += yyleng; return DO; }
"then"          {  position += yyleng; return THEN; }
"if"            {  position += yyleng; return IF; }
"else"          {  position += yyleng; return ELSE; }

"integer"       {  position += yyleng; return INTEGER; }
"real"          {  position += yyleng; return REAL; }
"boolean"       {  position += yyleng; return BOOLEAN; }
"true"          {  position += yyleng; return TRUE; }
"false"         {  position += yyleng; return FALSE; }

"="             {  position += yyleng; return EQUAL; }
"<"             {  position += yyleng; return LESS; }
">"             {  position += yyleng; return GREATER; }
">="            {  position += yyleng; return EQUAL_GREATER; }
"<="            {  position += yyleng; return EQUAL_LESS; }
"<>"            {  position += yyleng; return NOT_EQUAL; }
":="            {  position += yyleng; return ASSIGN; }

"."             {  position += yyleng; return DOT; }
","             {  position += yyleng; return COMMA; }
";"             {  position += yyleng; return SEMICOLON; }
":"             {  position += yyleng; return COLON; }

"+"             {  position += yyleng; return PLUS; }
"-"             {  position += yyleng; return MINUS; }
"*"             {  position += yyleng; return MULT; }
"/"             {  position += yyleng; return DIV_MATH; }
"div"           {  position += yyleng; return DIV_TEXT; }
"and"           {  position += yyleng; return AND; }
"mod"           {  position += yyleng; return MOD; }
"or"            {  position += yyleng; return OR; }

"("             {  position += yyleng; return BRACKET_ROUND_OPEN; }
")"             {  position += yyleng; return BRACKET_ROUND_CLOSE; }
"["             {  position += yyleng; return BRACKET_SQUARE_OPEN; }
"]"             {  position += yyleng; return BRACKET_SQUARE_CLOSE; }

{num}           {  position += yyleng; return NUM;}
{ident}         {  position += yyleng; return IDENT; }

.               {  position += yyleng; return ERROR;}

%%

int yywrap(void){
    return 1;
}