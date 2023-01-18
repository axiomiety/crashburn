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

struct X {
    X()  : count(X::total_count) {X::total_count += 1; cout << "X() " << count << endl;}
    X(const X&) : count(X::total_count) {X::total_count += 1; cout << "X(const &X) " << count << endl;}
    X& operator=(const X&) {count = X::total_count; X::total_count += 1; cout << "X& operator=(const X&) " << count << endl; return *this;}
    ~X() { cout << "~X " << count << endl;}
    uint count;
    static uint total_count;
};
uint X::total_count = 1;

void foo(const X& ref) {
    cout << "foo" << endl;
}

int main(int argc, char **argv)
{
    auto a = X();
    auto b = a;
    X c;

    {
        auto c = X(a);
    }
    vector<X> xvec = vector<X>{5};

    cout << "about to return" << endl;
    return EXIT_SUCCESS;
}