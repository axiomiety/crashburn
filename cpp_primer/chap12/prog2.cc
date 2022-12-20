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
using std::exception;
using std::make_shared;
using std::shared_ptr;

shared_ptr<vector<int>>
build_on_heap(int val)
{
    auto ivec = new vector<int>(val, 5);
    return shared_ptr<vector<int>>(ivec, [](auto p){ cout << "destroyed" << endl; delete p;});
}

int main(int argc, char **argv)
{
    auto ivec = build_on_heap(std::stoi(argv[1]));
    for (auto const& v: *ivec)
        cout << v << ",";
    cout << endl;
    return EXIT_SUCCESS;
}