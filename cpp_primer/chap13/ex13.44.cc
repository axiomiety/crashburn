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

class String
{
public:
    String()=default;
    String(const char*);            // copy ctor
    String(const String &);            // copy ctor
    String &operator=(const String &); // copy assignment ctor
    ~String();

private:
    static allocator<char> alloc;
    static allocator_traits<allocator<char>> at;
    void free();
};

allocator<string> String::alloc;
allocator_traits<allocator<string>> String::at;

StrVec::~StrVec() { free(); }

StrVec& StrVec::operator=(const StrVec &rhs)
{
    auto data= alloc_n_copy(rhs.begin(), rhs.end());
    free();
    elements = data.first;
    first_free = cap = data.second;
    return *this;
}

int main(int argc, char **argv)
{
    auto svec = vector<String>(), svec2 = vector<String>();
    svec.push_back("foobar");
    svec2 = svec;
    cout << "dtor" << endl;
    return EXIT_SUCCESS;
}