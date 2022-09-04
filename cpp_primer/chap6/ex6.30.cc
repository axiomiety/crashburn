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

bool str_subrange(const string &s1, const string &s2)
{
    if (s1.size() == s2.size())
        return s1 == s2;

    auto size = (s1.size() < s2.size()) ? s1.size() : s2.size();

    for (decltype(size) i = 0; i != size; ++i)
    {
        if (s1[i] != s2[i])
            return;
    }
}
int main(int argc, char **argv)
{
    string a = string("abc"), b = string("foo");
    cout << str_subrange(a, b) << endl;
}