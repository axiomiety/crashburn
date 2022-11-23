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
#include <sstream>

using std::back_inserter;
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::cerr;
using std::deque;
using std::end;
using std::endl;
using std::fill_n;
using std::forward_list;
using std::getline;
using std::ifstream;
using std::ofstream;
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
using namespace std::placeholders;

int main(int argc, char **argv)
{
    map<string, list<int>> lmap;
    ifstream infile("ex11.9.cc");
    string line;
    uint line_number = 1;
    while (std::getline(infile, line))
    {
        std::stringstream st(line);
        string word;
        while (st >> word)
        {
            lmap[word].push_back(line_number);
        }
        ++line_number;
    }
    for (const auto& lnum : lmap["while"])
        cout << lnum << endl;
    return EXIT_SUCCESS;
}