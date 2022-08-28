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

void swap(int **x, int **y)
{
    int *tmp = *y;
    *y = *x;
    *x = tmp;
}

int main()
{
    int a = 10, b = 22;
    int *pa = &a, *pb = &b;
    cout << std::hex << pa << " : " << std::hex << pb << endl;
    swap(&pa, &pb);
    cout << std::hex << pa << " : " << std::hex << pb << endl;
    cout << std::dec << a << " : " << b << endl;
    cout << *pa << " : " << *pb << endl;
    a = 11, b = 23;
    cout << *pa << " : " << *pb << endl;
    return 0;
}