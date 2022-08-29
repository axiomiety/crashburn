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

int main(int argc, char **argv)
{
    string concat = string(argv[1]) + string(argv[2]);
    cout << concat << endl;
    for (int c = 1; c <= argc; ++c)
        cout << argv[c] << endl;
    return 0;
}