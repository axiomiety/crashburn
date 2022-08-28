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

int biggest(int x, const int *y)
{
    if (x >= *y)
        return x;
    return *y;
}

int main()
{
    int a = 10, b = 22;
    cout << biggest(a, &b) << endl;
    return 0;
}