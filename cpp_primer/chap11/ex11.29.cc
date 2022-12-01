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
using std::multimap;
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;
using namespace std::placeholders;

int main(int argc, char **argv)
{
    multimap<string, int> mmap;
    mmap.insert({"white", 1});
    mmap.insert({"white", 3});
    mmap.insert({"white", 5});
    mmap.insert({"red", 11});
    mmap.insert({"red", 22});


    string missing_key("blue");
    auto lb = mmap.lower_bound(missing_key);
    auto ub = mmap.upper_bound(missing_key);
    auto er = mmap.equal_range(missing_key);

    assert(ub == lb);
    assert(er.first == lb);
    assert(er.second == ub);
    return EXIT_SUCCESS;
}