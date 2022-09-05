#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <cassert>
#include "Chapter6.h"
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::map;
using std::string;
using std::vector;

// compile with:
// ❯ gcc -D NDEBUG prog3.cc -std=c++17 -lstdc++ -I. -o prog -fpermissive && ./prog
void reset(int &i)
{
#ifndef NDEBUG
    cerr << __func__ << ":" << __LINE__ << endl;
    assert(0);
#endif
    i = 0;
}
int main()
{
    int k = 23;
    int &r = k;
    reset(r);
    cout << k << endl;
    return 0;
}