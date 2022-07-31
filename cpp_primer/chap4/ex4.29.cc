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
    int x[10];
    int *p = x;
    cout << sizeof(x)/sizeof(*x) << endl;
    // on my 64b machine, that's 8 bytes - so 64bit
    cout << sizeof(int *) << endl;
    cout << sizeof(p)/sizeof(*p) << endl;
}