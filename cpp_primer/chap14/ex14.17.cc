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
    friend istream& operator>>(istream& in, Foo& item);
    friend Foo operator+(const Foo& lhs, const Foo& rhs);
    friend bool operator==(const Foo& lhs, const Foo& rhs);
public:
    Foo(vector<int> data={}) : data(data) { }
    Foo sorted() &&;
    Foo sorted() const &;
    string toString() const;
    Foo& operator+=(const Foo&);
private:
    vector<int> data;
};

ostream& operator<<(ostream &os, const Foo &item) {
    for (const auto& d : item.data)
        os << d << ",";
    return os;
}

istream& operator>>(istream& in, Foo& item){
    istream_iterator<int> ii(in), eof;
    copy(ii, eof, back_inserter(item.data));
    return in;
}

Foo operator+(const Foo& lhs, const Foo& rhs){
    auto ret = Foo();
    ret.data.insert(ret.data.end(), lhs.data.cbegin(), lhs.data.cend());
    ret.data.insert(ret.data.end(), rhs.data.cbegin(), rhs.data.cend());
    return ret;
}

bool operator==(const Foo& lhs, const Foo& rhs){
    return lhs.data == rhs.data;
}

bool operator!=(const Foo& lhs, const Foo& rhs){
    return !(lhs == rhs);
}

// can't have this return *this if it's not a member fn!
Foo& Foo::operator+=(const Foo& rhs)
{

    this->data.insert(this->data.end(), rhs.data.cbegin(), rhs.data.cend());
    return *this;
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

    istringstream cin("1 1 2 2 3 3 4 4 5 5");
    auto f2 = Foo();
    while (cin)
        cin >> f2;

    cout << f2 << endl;
    auto f3 = Foo({1,2}) + Foo({3,4});
    cout << f3 << endl;

    f3 += Foo({5,6,7});
    cout << f3 << endl;

    cout << (f2 == f2) << endl;
    cout << (f3 != f3) << endl;

    return EXIT_SUCCESS;
}