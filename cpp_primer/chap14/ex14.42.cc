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

int main(int argc, char **argv)
{
    vector<int> ivec = {10, 2000, 50, 60, 1500};
    auto c = count_if(ivec.cbegin(), ivec.cend(), [](int a){ return a > 1024; });
    cout << c << endl;
    # we can use bind too, but as i understand it the advice is to replace this with lambda nowadays
    cout << count_if(ivec.cbegin(), ivec.cend(), bind(greater<int>(), placeholders::_1, 1024)) << endl;

    vector<string> svec = {"pooh", "pooh", "bear", "honey"};
    auto it = find_if_not(svec.cbegin(), svec.cend(), [](string s){ return s == "pooh";});
    cout << *it << endl;

    vector<int> nivec;
    transform(ivec.cbegin(), ivec.cend(), back_inserter(nivec), [](int a){ return a*2;});
    for (const auto i: nivec)
        cout << i << " ";
    cout << endl;
    return EXIT_SUCCESS;
}