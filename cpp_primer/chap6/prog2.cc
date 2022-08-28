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