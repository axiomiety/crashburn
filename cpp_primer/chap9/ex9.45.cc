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
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::deque;
using std::end;
using std::endl;
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

void search_and_replace(string &s, const string &old_val, const string &new_val)
{
    auto s_iter = s.begin();
    auto match_start = old_val.cend();
    auto old_val_iter = old_val.cbegin();
    while (s_iter != s.end())
    {
        cout << "s_iter is: " << *s_iter << endl;
        if (*s_iter == *old_val_iter)
        {
            if (match_start == old_val.cend())
                match_start = s_iter;
            ++s_iter;
            ++old_val_iter;
        }
        else
        {
            if (old_val_iter == old_val.cend())
            {
                // full match found
                // cout << "before: " << s << endl;
                s_iter = s.erase(match_start, match_start + old_val.size());
                // cout << "after: " << s << endl;
                s_iter = s.insert(s_iter, new_val.begin(), new_val.end()); // new_val);
                // cout << "insert: " << s << endl;
                s_iter += new_val.size();
            }
            match_start = old_val.cend();
            ++s_iter;
            old_val_iter = old_val.cbegin();
        }
    }
}

int main(int argc, char **argv)
{
    string s("the quick brown fox jumps over the lazy dog");
    search_and_replace(s, "the", "foo");
    cout << s << endl;
    return EXIT_SUCCESS;
}