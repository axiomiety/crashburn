#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
#include <forward_list>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
#include <memory>
#include <algorithm>
#include <iterator>
#include <functional>
#include <map>
#include <set>
#include <cassert>
#include <memory>

using namespace std;

// can't get it to segfault!
// ❯ gcc ex12.24.cc -lstdc++ -std=c++20 -I. -o prog && ./prog < <(cat /dev/urandom | LC_ALL=C tr -dc 'a-zA-Z0-9' | fold -w 500000 | head -n 1)
int main(int argc, char **argv)
{
    char* arr = new char[4]{'h','e','l'};
    size_t count = 0;
    char c;
    while (cin >> c)
    {
        *(arr + count) = c;
        ++count;
    }
    //cout << arr << endl;
    delete[] arr;
    return EXIT_SUCCESS;
}