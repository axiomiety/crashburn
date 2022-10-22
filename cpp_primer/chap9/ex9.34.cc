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
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::deque;
using std::end;
using std::endl;
using std::forward_list;
using std::getline;
using std::ifstream;
using std::initializer_list;
using std::istream;
using std::istringstream;
using std::iterator;
using std::list;
using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;

int main(int argc, char **argv)
{
    vector<int> ivec = {0, 1, 1, 2, 3, 5, 8, 13, 21, 55, 89};

    auto iter = ivec.begin();
    while (iter != ivec.end())
    {
        if (*iter % 2)
            iter = ivec.insert(iter, *iter) + 1;
        ++iter;
    }

    for (const auto &it : ivec)
        cout << it << " ";
    cout << endl;

    return EXIT_SUCCESS;
}