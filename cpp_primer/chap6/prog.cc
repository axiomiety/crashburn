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

size_t count_calls()
{
    static size_t ctr = 0;
    return ++ctr;
}
int main()
{
    for (auto i = 0; i != 10; ++i)
        cout << count_calls() << endl;
    return 0;
}