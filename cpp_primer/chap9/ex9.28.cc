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

void func(forward_list<string> *fwlist, string s1, string s2)
{
    auto prev = fwlist->before_begin();
    auto curr = fwlist->begin();
    bool found = false;

    while (curr != fwlist->end())
    {
        if (*curr == s1)
        {
            fwlist->insert_after(curr, s2);
            ++curr;
            found = true;
        }
        prev == curr;
        ++curr;
    }
    if (!found)
    {
        fwlist->insert_after(prev, s2);
    }
}

int main(int argc, char **argv)
{
    forward_list<string> fwlist = {"the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"};

    func(&fwlist, "the", "silly");
    for (const auto &it : fwlist)
        cout << it << " ";
    cout << endl;

    fwlist = {};
    func(&fwlist, "the", "silly");
    for (const auto &it : fwlist)
        cout << it << " ";
    cout << endl;

    return EXIT_SUCCESS;
}