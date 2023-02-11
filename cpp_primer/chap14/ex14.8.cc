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
    friend ostream& operator<<(ostream &os, const Foo &item);
public:
    Foo(vector<int> data) : data(data) { }
    Foo sorted() &&;
    Foo sorted() const &;
    string toString() const;
private:
    vector<int> data;
};

ostream& operator<<(ostream &os, const Foo &item) {
    for (const auto& d : item.data)
        os << d << ",";
    return os;
}

Foo
Foo::sorted() &&
{
    cout << "rval sort" << endl;
    sort(data.begin(), data.end());
    return *this;
}
Foo Foo::sorted() const& {
    cout << "lval sort" << endl;
    Foo ret(*this);
    sort(ret.data.begin(), ret.data.end());
    return ret;
}

int main(int argc, char **argv)
{
    auto f1 = Foo({1,2,3});
    f1.sorted();
    cout << f1 << endl;
    cout << Foo({3,2,1}).sorted() << endl;
    return EXIT_SUCCESS;
}