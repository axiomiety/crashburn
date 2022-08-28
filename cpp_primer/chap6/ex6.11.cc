#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include "Chapter6.h"
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::map;
using std::string;
using std::vector;

void reset(int &i)
{
    i = 1;
}
void reset(int *i)
{
    *i = 0;
}
int main()
{
    int k = 23;
    int &r = k;
    // reset(r);
    reset(k);
    cout << k << endl;
    cout << r << endl;
    int j = 12;
    reset(&j);
    cout << j << endl;
    return 0;
}