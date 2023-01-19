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

struct numbered {
    numbered() {
        mysn = rand();
    }
    numbered(const numbered&) {
        mysn = rand();
    }
    uint mysn;
};

void f(numbered s) {
    cout << s.mysn << endl;
}

void ff(const numbered& s) {
    cout << s.mysn << endl;
}

int main(int argc, char **argv)
{
    numbered a, b=a, c=b;
    f(a);
    ff(a);
    f(b);
    f(c);
    return EXIT_SUCCESS;
}