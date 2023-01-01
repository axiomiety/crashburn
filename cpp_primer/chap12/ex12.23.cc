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
#include <functional>
#include <map>
#include <set>
#include <cassert>
#include <memory>

using namespace std;

char* concat(const char* s1, const char* s2)
{
    size_t l = strlen(s1) + strlen(s2) + 1;
    char* arr = new char[l];
    strcpy(arr, s1);
    strcpy(arr+strlen(s1), s2);
    return arr;
}

string* concat(const string&& s1, const string&& s2)
{
    return new string(s1 + s2);
}

int main(int argc, char **argv)
{
    const char* s1 = "foo";
    const char* s2 = "bar";
    auto a = concat(s1, s2);
    cout << a << endl;
    delete[] a;
    auto s = concat("foo", "bar");
    cout << s << endl;
    delete s;
    return EXIT_SUCCESS;
}