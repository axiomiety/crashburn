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

shared_ptr<vector<int>>
build_on_heap(int val)
{
    auto ivec = new vector<int>(val, 5);
    return shared_ptr<vector<int>>(ivec, [](auto p){ cout << "destroyed" << endl; delete p;});
}

shared_ptr<vector<int>>
wptr_lock(weak_ptr<vector<int>> wptr)
{
    auto ret = wptr.lock();
    if (!ret)
        throw runtime_error("couldn't lock");
    cout << "use count after locking is: " << ret.use_count() << endl;
    return ret;
}

int main(int argc, char **argv)
{
    auto ivec = build_on_heap(3);
    cout << "use count is: " << ivec.use_count() << endl;
    weak_ptr<vector<int>> wptr(ivec);
    cout << "use count after weak_ptr is: " << ivec.use_count() << endl;
    auto sptr = wptr_lock(wptr);
    cout << "use count is: " << ivec.use_count() << endl;
    cout << "use count (sptr) is: " << sptr.use_count() << endl;
    return EXIT_SUCCESS;
}