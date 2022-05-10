
#include <iostream>
#include <chrono>

extern int yyparse();

using namespace std;

int main(void){
    cout << "+-------------------------------------------------------------------+" << endl;
    cout << "Parsing new File" << endl;
    auto start = chrono::high_resolution_clock::now();

    yyparse();
    auto stop = chrono::high_resolution_clock::now();
    auto duration = chrono::duration_cast<chrono::microseconds>(stop - start);

    cout << "Parsed new File" << endl;
    cout << "\tExecution time: " << duration.count() << " ms " << endl;
    cout << "+-------------------------------------------------------------------+" << endl << endl;
}