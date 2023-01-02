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

int main(int argc, char **argv)
{
    // can't use methods on the allocator from C++20
    allocator<string> alloc;
    allocator_traits<allocator<string>> at;
    uint n = 10;
    auto p = at.allocate(alloc, n);
    string* q = p;
    string s;
    while (cin >> s && q != p + n)
        at.construct(alloc, q++, s);
    
    while (q!=p)
        at.destroy(alloc, --q);
    
    return EXIT_SUCCESS;
}