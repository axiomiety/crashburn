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

bool is_str_length_greater_than(const string &s, uint sz)
{
    return s.size() > sz;
}

int main(int argc, char **argv)
{
    vector<string> svec;
    string token;
    while (cin >> token)
        svec.push_back(token);

    auto len_gt_6 = std::bind(is_str_length_greater_than, _1, 3);
    cout << count_if(svec.cbegin(), svec.cend(), len_gt_6) << endl;
    return EXIT_SUCCESS;
}