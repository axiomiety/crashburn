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


class StrBlob {
    friend class StrBlobPtr;
    public:
        typedef vector<string>::size_type size_type;
        StrBlob();
        StrBlob(initializer_list<string> il);
        size_type size() const { return data->size(); }
        bool empty() const { return data->empty(); }
        void push_back(const string& t) { data->push_back(t); }
        // move semantics, just because
        void push_back(const string&& t) { cout << "rvalue ref" << endl; data->push_back(move(t)); }
        void pop_back();
        string& front();
        string & back();
    private:
        shared_ptr<vector<string>> data;
        void check(size_type i, const string& msg) const;
};

StrBlob::StrBlob(): data(make_shared<vector<string>>()) { }
StrBlob::StrBlob(initializer_list<string> il): data(make_shared<vector<string>>(il)) { }

void StrBlob::check(size_type i, const string& msg) const
{
    if (i >= data->size())
        throw std::out_of_range(msg);
}

string& StrBlob::front()
{
    check(0, "front on emtpy StrBlobl");
    return data->front();
}

string& StrBlob::back()
{
    check(0, "back on emtpy StrBlobl");
    return data->back();
}

void StrBlob::pop_back()
{
    check(0, "pop_back on empty StrBlob");
    data->pop_back();
}

class StrBlobPtr {
    public:
        StrBlobPtr(): curr(0) {}
        StrBlobPtr(StrBlob& a, size_t sz=0): wptr(a.data), curr(sz) {}
        string& deref() const;
        StrBlobPtr& incr();

    private:
        shared_ptr<vector<string>> check(size_t, const string&) const;
        weak_ptr<vector<string>> wptr;
        size_t curr;
};

shared_ptr<vector<string>> StrBlobPtr::check(size_t i, const string& msg) const
{
    auto ret = wptr.lock();
    if (!ret)
        throw runtime_error("unbound StrBlobPtr, lock failed");
    if (i >= ret->size())
        throw out_of_range(msg);
    return ret;
}

string& StrBlobPtr::deref() const
{
    auto p = check(curr, "dereference past end");
    return (*p)[curr];
}

StrBlobPtr& StrBlobPtr::incr()
{
    check(curr, "increment past end of StrBlobPtr");
    ++curr;
    return *this;
}

int main(int argc, char **argv)
{
    auto blob = StrBlob();
    ifstream in(argv[1]);
    string line;
    while (getline(in, line))
    {
        blob.push_back(line);
    }
    auto p = StrBlobPtr(blob);
    while (1)
    {
        try
        {
            cout << p.deref() << endl;
            p.incr();
        }
        catch(const out_of_range& e)
        {
            // nothing for us to do
            break;
        }
        
    }
    return EXIT_SUCCESS;
}