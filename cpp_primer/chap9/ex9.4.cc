#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
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

bool is_in_range(vector<int>::iterator start, vector<int>::iterator end, int val)
{
    for (auto it = start; it != end; ++it)
    {
        if ((*it) == val)
            return true;
    }
    return false;
}

int main(int argc, char **argv)
{
    vector<int> ivec{1, 2, 3, 4, 5};
    cout << is_in_range(ivec.begin(), ivec.end(), 2) << endl;
    cout << is_in_range(ivec.begin() + 2, ivec.end(), 2) << endl;
    return EXIT_SUCCESS;
}