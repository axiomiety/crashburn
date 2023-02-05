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

class Foo
{
public:
    Foo() = default;
    Foo(int i) : x(i) {}
    Foo(const Foo&);
    void print(ostream&);
private:
    int x;
};

Foo::Foo(const Foo&f)
{
    cout << "copy ctor called" << endl;
    x = f.x;
}
void
Foo::print(ostream& out)
{
    out << x << endl;
}


class Bar
{
public:
    Bar() = default;
    Bar(int i) : x(i) {}
    Bar(const Bar&);
    Bar(const Bar&&) noexcept;
    void print(ostream&);
private:
    int x;
};

Bar::Bar(const Bar& f)
{
    cout << "copy ctor called" << endl;
    x = f.x;
}
Bar::Bar(const Bar&& f) noexcept
{
    cout << "move ctor called" << endl;
    x = move(f.x);
}
void
Bar::print(ostream& out)
{
    out << x << endl;
}
int main(int argc, char **argv)
{
    auto a = Foo(), b = Foo(1);
    auto c = Foo(b);
    a.print(cout);
    b.print(cout);
    c.print(cout);
    vector<Foo> fvec(2);
    fvec.emplace_back(3);

    auto d = Bar();
    auto e = Bar(d);
    Bar f;
    auto g = Bar(move(f));
    auto h = Bar(move(Bar()));
    vector<Bar> bvec(2);
    bvec.emplace_back(3);
    return EXIT_SUCCESS;
}