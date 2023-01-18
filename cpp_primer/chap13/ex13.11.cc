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

class HasPtr
{
    public:
        HasPtr(const string &s= string()): ps(new string(s)), i(0) {}
        HasPtr(const HasPtr& c) : ps(new string(*c.ps)), i(c.i) {}
        HasPtr& operator=(const HasPtr& c) {
            ps = new string(*c.ps);
            i = c.i;
            return *this;
            }
        void print() { cout << ps << "," << *ps << "," << i << endl;}
        ~HasPtr() {
            delete ps;
        }
    private:
        string* ps;
        int i;
};

int main(int argc, char **argv)
{
    HasPtr b;
    const string s("foobar");
    auto a = HasPtr(s);
    {
        b = HasPtr(a);
    }
    auto c = a;

    a.print();
    b.print();
    c.print();

    return EXIT_SUCCESS;
}