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

int main(int argc, char **argv)
{
    forward_list<int> fwlist = {0, 1, 1, 2, 3, 5, 8, 13, 21, 55, 89};

    auto prev = fwlist.before_begin();
    auto curr = fwlist.begin();

    while (curr != fwlist.end())
    {
        if (*curr % 2)
            curr = fwlist.erase_after(prev);
        else
        {
            prev = curr;
            ++curr;
        }
    }

    for (const auto &it : fwlist)
        cout << it << " ";
    cout << endl;
    return EXIT_SUCCESS;
}