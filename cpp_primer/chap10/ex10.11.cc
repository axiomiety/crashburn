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

bool is_shorter(const string &s1, const string &s2)
{
    return s1.size() < s2.size();
}

void elim_dupes(vector<string> &svec, void (*sort_fn)(vector<string>::iterator, vector<string>::iterator, bool (*)(const string&, const string&)) )
{
    // first we sort
    sort_fn(svec.begin(), svec.end(), is_shorter);
    // get all the dupes at the end
    auto end_unique = unique(svec.begin(), svec.end());
    // get rid of the dupes
    svec.erase(end_unique, svec.end());
}

int main(int argc, char **argv)
{
    vector<string> svec;
    string token;
    while (cin >> token)
        svec.push_back(token);

    cout << "before: " << endl;
    for (const auto &i : svec)
    {
        cout << i << " ";
    }
    cout << endl;
    elim_dupes(svec, std::stable_sort);
    cout << "after: " << endl;
    for (const auto &i : svec)
    {
        cout << i << " ";
    }
    return EXIT_SUCCESS;
}