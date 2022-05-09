
#include <iostream>

extern int yyparse();

int main(void){

    std::cout << "Parsing new File" << std::endl;

    yyparse();

    std::cout << "Parsed new File" << std::endl;

}