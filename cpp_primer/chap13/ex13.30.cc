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
friend void swap(HasPtr&, HasPtr&);
public:
    HasPtr(const string &s, int j) : ps(new string(s)), i(j), count(new size_t(1))
    {
        cout << "ctor str+int" << endl;
    }
    HasPtr(const string &s = string()) : ps(new string(s)), i(0), count(new size_t(1))
    {
        cout << "ctor str-only" << endl;
    }
    HasPtr(const HasPtr &c) : ps(new string(*c.ps)), i(c.i), count(c.count) { ++*count; }
    // HasPtr &operator=(const HasPtr &c)
    // {
    //     ps = new string(*c.ps);
    //     i = c.i;
    //     return *this;
    // }
    HasPtr &operator=(HasPtr rhs)
    {
        swap(*this, rhs);
        return *this;
    }
    void print() { cout << ps << "," << *ps << "," << i << endl; }
    bool operator<(const HasPtr &rhs) const
    {
        return i < rhs.i;
    }
    ~HasPtr()
    {
        if (--*count == 0)
        {
            cout << "count is " << *count << endl;
            delete ps;
            delete count;
        }
    }

private:
    string *ps;
    int i;
    size_t *count;
};

void swap(HasPtr &lhs, HasPtr &rhs)
{
    cout << "in swap" << endl;
    using std::swap;
    swap(lhs.i, rhs.i);
    swap(lhs.ps, rhs.ps);
}
int main(int argc, char **argv)
{
    HasPtr b;
    const string s("foobar");
    auto a = HasPtr(s);
    {
        b = HasPtr(static_cast<const HasPtr&>(a));
    }
    auto c = a;

    a.print();
    b.print();
    c.print();

    vector<HasPtr> vec = vector<HasPtr>{4};
    vec.emplace_back("foo", 4);
    vec.emplace_back("foo", 1);
    vec.emplace_back("foo", 2);
    sort(vec.begin(), vec.end());

    return EXIT_SUCCESS;
}