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

bool compare(const list<int> &ilist, const vector<int> &ivec)
{
    auto ilistiter = ilist.cbegin();
    auto ivectoriter = ivec.cbegin();
    while (ilistiter != ilist.cend() && ivectoriter != ivec.cend())
    {
        if (*ilistiter == *ivectoriter)
        {
            ++ilistiter;
            ++ivectoriter;
            continue;
        }
        else
            return false;
    }
    return ilistiter == ilist.cend() && ivectoriter == ivec.cend();
}
int main(int argc, char **argv)
{
    list<int> ilist = {1, 2, 3};
    vector<int> ivec = {1, 2, 3, 4};
    cout << compare(ilist, ivec) << endl;
    return EXIT_SUCCESS;
}