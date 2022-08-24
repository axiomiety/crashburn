#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::map;
using std::string;
using std::vector;

void swap_ints(int *a, int *b)
{
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int main()
{
    int a = 1, b = 2;
    int *p = &a, *q = &b;
    cout << *p << " " << *q << endl;
    swap_ints(p, q);
    cout << *p << " " << *q << endl;
    return 0;
}