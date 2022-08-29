#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::initializer_list;
using std::map;
using std::string;
using std::vector;

int sum(initializer_list<int> il)
{
    int total = 0;
    for (const auto &i : il)
        total += i;
    return total;
}

int main(int argc, char **argv)
{
    initializer_list<int> il{1, 2, 3};
    cout << sum(il) << endl;
    return EXIT_SUCCESS;
}