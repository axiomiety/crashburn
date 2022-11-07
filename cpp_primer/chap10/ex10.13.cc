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

typedef bool (*compare_func)(const string &, const string &);

bool is_long_enough(const string &s)
{
    return s.size() > 3;
}

int main(int argc, char **argv)
{
    vector<string> svec;
    string token;
    while (cin >> token)
        svec.push_back(token);

    auto cutoff = std::partition(svec.begin(), svec.end(), is_long_enough);
    cout << " > 3: " << endl;
    for (auto it = svec.begin(); it != cutoff; ++it)
    {
        cout << *it << " ";
    }
    cout << endl;
    cout << " <= 3: " << endl;
    for (auto it = cutoff; it != svec.end(); ++it)
    {
        cout << *it << " ";
    }
    return EXIT_SUCCESS;
}