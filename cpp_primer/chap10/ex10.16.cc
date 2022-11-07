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

int main(int argc, char **argv)
{
    vector<string> svec;
    string token;
    while (cin >> token)
        svec.push_back(token);

    vector<string>::size_type sz = 3;
    sort(svec.begin(), svec.end());
    auto end_unique = unique(svec.begin(), svec.end());
    svec.erase(end_unique, svec.end());

    stable_sort(svec.begin(), svec.end(), [](const string &s1, const string &s2)
                { return s1.size() < s2.size(); });

    auto pred = [sz](const string &s) { return s.size() > sz; };
    auto wc = std::find_if(svec.begin(), svec.end(), pred);
    // we essentially ordered by size, so wc is the first element
    // for which the predicate is true
    auto count = svec.end() - wc;
    cout << "found " << count << " elements satisfying the predicate" << endl;
    std::for_each(wc, svec.end(), [](const string &s)
                  { cout << s << " "; });
    cout << endl;
    // for all the elements less than
    std::for_each(svec.begin(), wc, [](const string &s)
                  { cout << s << " "; });

    return EXIT_SUCCESS;
}