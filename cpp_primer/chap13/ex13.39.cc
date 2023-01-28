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

class StrVec
{
public:
    StrVec() : elements(nullptr), first_free(nullptr), cap(nullptr) {}
    StrVec(const StrVec &);          // copy ctor
    StrVec &operator=(const StrVec&); // copy assignment ctor
    ~StrVec();

    void push_back(const string &);
    size_t size() const { return first_free - elements; }
    size_t capacity() const { return cap - elements; }
    string *begin() const { return elements; }
    string *end() const { return first_free; }

private:
    static allocator<string> alloc;
    void chk_n_alloc()
    {
        if (size() == capacity())
            reallocate();
    };
    pair<string*, string*> alloc_n_copy(const string*, const string*);
    void free();
    void relallocate();
    string *elements;
    string *first_free;
    string *cap;
};

allocator<string> StrVec::alloc;

void StrVec::push_back(const string& s)
{
    chk_n_alloc();
    alloc.construct(first_free++, s);
}

pair<string*, string*>
StrVec::alloc_n_copy(const string* b, const string* e)
{
    auto data = alloc.allocate(e-b);
    return {data, uninitialized_copy(b, e, data);}
}

int main(int argc, char **argv)
{
    return EXIT_SUCCESS;
}