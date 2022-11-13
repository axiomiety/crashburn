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
    vector<string> svec;
    std::ifstream in("ex10.29.cc");
    std::istream_iterator<string> str_in(in), eof;
    std::copy(str_in, eof, std::back_inserter(svec));

    std::ostream_iterator<string> out_iter(cout, " ");
    std::copy(svec.begin(), svec.end(), out_iter);
    cout << endl;
    return EXIT_SUCCESS;
}