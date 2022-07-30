#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;
using std::vector;

int main()
{
    int i;
    while (cin >> i && i != 42)
        cout << "ain't it" << endl;

    return 0;
}