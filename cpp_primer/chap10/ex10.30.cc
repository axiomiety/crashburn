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
using std::back_inserter;
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::deque;
using std::end;
using std::endl;
using std::fill_n;
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
using namespace std::placeholders;

int main(int argc, char **argv)
{
    std::istream_iterator<int> int_in(cin), eof;
    std::ostream_iterator<int> out_iter(cout, " ");
    // same as vector<int> ivec(int_in, eof);
    vector<int> ivec;
    std::copy(int_in, eof, std::back_inserter(ivec));
    std::sort(ivec.begin(), ivec.end());
    std::copy(ivec.cbegin(), ivec.cend(), out_iter);
    cout << endl;
    return EXIT_SUCCESS;
}