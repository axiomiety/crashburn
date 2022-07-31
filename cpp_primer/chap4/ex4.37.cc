#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;
using std::vector;
using std::string;

int main()
{
    int i;
    double d;
    const string *ps;
    char *pc;
    void *pv;
    // ended up looking at https://github.com/mrmanago/cpp_primer_5th_exercises/blob/master/ch04/4.37.md
    pv = (void*) ps;
    pv = static_cast<void*>(const_cast<string*>(ps));
    i = int(*pc);
    i = static_cast<int>(*pc);
    pv = &d;
    pv = static_cast<void*>(&d);
    pc = (char*) pv;
    pc = static_cast<char*>(pv);
    cout << "done" << endl;
}