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

unsigned int fact(int n)
{
    unsigned int total = n;
    while (--n)
    {
        total *= n;
    }
    return total;
}

int main()
{
    int a;
    cout << "Enter an integer: ";
    while (cin >> a)
    {
        cout << a << " factorial is " << fact(a) << endl;
        cout << "Enter an integer: ";
    }
    return 0;
}