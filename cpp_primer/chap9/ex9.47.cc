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

void myfind(const string &s)
{
    string numbers("0123456789");
    string::size_type pos = 0;
    while ((pos = s.find_first_of(numbers, pos)) != string::npos)
    {
        cout << s[pos];
        ++pos;
    }
    cout << endl;
    pos = 0;
    while ((pos = s.find_first_not_of(numbers, pos)) != string::npos)
    {
        cout << s[pos];
        ++pos;
    }
    cout << endl;
}

int main(int argc, char **argv)
{
    string s("ab2c3d7R4E6");
    myfind(s);
    return EXIT_SUCCESS;
}