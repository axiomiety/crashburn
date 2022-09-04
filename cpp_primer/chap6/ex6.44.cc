#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::initializer_list;
using std::map;
using std::string;
using std::vector;

inline bool isShorter(const string &s1, const string &s2)
{
    return s1.size() < s2.size();
}
int main(int argc, char **argv)
{
    cout << (isShorter("foo", "foobar") ? "true" : "false") << endl;
    return EXIT_SUCCESS;
}