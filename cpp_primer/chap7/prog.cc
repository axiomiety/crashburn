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

class X
{

public:
    int i, j;
    X(int val) : j(val), i(j) {}
};

int main()
{
    auto x = X(3);
    cout << x.i << endl;
    cout << x.j << endl;
}