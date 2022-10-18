#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
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
    int ia[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 55, 89};
    vector<int> ivec(begin(ia), end(ia));
    list<int> ilist(begin(ia), end(ia));

    auto itlist = ilist.begin();
    while (itlist != ilist.end())
    {
        if (*itlist % 2)
            ++itlist;
        else
            itlist = ilist.erase(itlist);
    }
    auto itvec = ivec.begin();
    while (itvec != ivec.end())
    {
        if (*itvec % 2)
            itvec = ivec.erase(itvec);
        else
            ++itvec;
    }

    for (const auto &it : ilist)
        cout << it << " ";
    cout << endl;
    for (const auto &it : ivec)
        cout << it << " ";
    cout << endl;
    return EXIT_SUCCESS;
}