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
    Foo(vector<int> data) : data(data) { }
    Foo sorted() &&;
    Foo sorted() const &;
    string toString() const;
private:
    vector<int> data;
};

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

string Foo::toString() const {
    stringstream ss;
    for (const auto &i : data) {
        ss << i << ",";
    }
    return ss.str();
}

int main(int argc, char **argv)
{
    auto f1 = Foo({1,2,3});
    f1.sorted();
    cout << f1.toString() << endl;
    cout << Foo({3,2,1}).sorted().toString() << endl;
    return EXIT_SUCCESS;
}