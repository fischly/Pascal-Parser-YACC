#include <stdio.h>
#include <stdlib.h>
#include <time.h>       // for clock_t, clock(), CLOCKS_PER_SEC
#include <unistd.h>     // for sleep()

extern void printMetaInfo();
extern int yyparse();

int main(void){
    printf("+--------------------------------------------------------------------------------------------+\n");
    printf("PARSING NEW FILE \n");
    printf("(LN | FR - TO) LEX::TOKEN::VALUE \n");
    printf("+--------------------------------------------------------------------------------------------+\n");

    double time_spent = 0.0;
    clock_t begin = clock();
    int parse = yyparse();
    clock_t end = clock();
    time_spent += (double)(end - begin) / CLOCKS_PER_SEC;

    printf("Parsing terminated.\n");
    if(!parse){
        printf("The program is consistent with the aligned rules.\n");
    }
    printf("\tParsing Time: %fs\n", time_spent);

    return 0;
}