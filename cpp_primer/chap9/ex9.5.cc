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

vector<int>::iterator is_in_range(vector<int>::iterator start, vector<int>::iterator end, int val)
{
    for (auto it = start; it != end; ++it)
    {
        if ((*it) == val)
            return it;
    }
    return end;
}

int main(int argc, char **argv)
{
    vector<int> ivec{1, 2, 3, 4, 5};
    auto exists = is_in_range(ivec.begin(), ivec.end(), 2);
    if (exists != ivec.end())
        cout << "found! " << *exists << endl;
    exists = is_in_range(ivec.begin() + 2, ivec.end(), 2);
    if (exists == ivec.end())
        cout << "not found" << endl;
    return EXIT_SUCCESS;
}