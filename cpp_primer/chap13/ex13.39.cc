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
#include <utility>


using namespace std;

class StrVec
{
public:
    StrVec() : elements(nullptr), first_free(nullptr), cap(nullptr) {}
    StrVec(const StrVec &);            // copy ctor
    StrVec &operator=(const StrVec &); // copy assignment ctor
    ~StrVec();

    void push_back(const string &);
    size_t size() const { return first_free - elements; }
    size_t capacity() const { return cap - elements; }
    string *begin() const { return elements; }
    string *end() const { return first_free; }

private:
    static allocator<string> alloc;
    static allocator_traits<allocator<string>> at;
    void chk_n_alloc()
    {
        if (size() == capacity())
            reallocate();
    };
    pair<string *, string *> alloc_n_copy(const string *, const string *);
    void free();
    void reallocate();
    string *elements;
    string *first_free;
    string *cap;
};

allocator<string> StrVec::alloc;
allocator_traits<allocator<string>> StrVec::at;

void StrVec::push_back(const string &s)
{
    chk_n_alloc();
    at.construct(alloc, first_free++, s);
}

pair<string *, string *>
StrVec::alloc_n_copy(const string *b, const string *e)
{
    auto data = at.allocate(alloc, e - b);
    return {data, uninitialized_copy(b, e, data)};
}

void StrVec::free()
{
    cout << "free called" << endl;
    if (elements)
    {
        for (auto p = first_free; p != elements;)
            at.destroy(alloc, --p);
        at.deallocate(alloc, elements, cap - elements);
    }
}
StrVec::StrVec(const StrVec &s)
{
    auto newdata = alloc_n_copy(s.begin(), s.end());
    elements = newdata.first;
    first_free = cap = newdata.second;
}

StrVec::~StrVec() { free(); }

StrVec& StrVec::operator=(const StrVec &rhs)
{
    auto data= alloc_n_copy(rhs.begin(), rhs.end());
    free();
    elements = data.first;
    first_free = cap = data.second;
    return *this;
}

void StrVec::reallocate()
{
    cout << "realloc called" << endl;
    auto newcapacity = size() ? 2*size() : 1;
    auto newdata = alloc.allocate(newcapacity);
    auto dest = newdata;
    auto elem = elements;
    for (size_t i = 0; i != size(); ++i)
        at.construct(alloc, dest++, move(*elem++));
    free();
    elements = newdata;
    first_free = dest;
    cap = elements + newcapacity;
}

int main(int argc, char **argv)
{
    auto svec = StrVec(), svec2 = StrVec();
    svec.push_back("foobar");
    svec2 = svec;
    cout << "dtor" << endl;
    return EXIT_SUCCESS;

 