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

using std::back_inserter;
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::cerr;
using std::deque;
using std::end;
using std::endl;
using std::fill_n;
using std::forward_list;
using std::getline;
using std::ifstream;
using std::ofstream;
using std::initializer_list;
using std::istream;
using std::istringstream;
using std::iterator;
using std::list;
using std::map;
using std::multimap;
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;
using namespace std::placeholders;
using std::exception;

using shivec = std::shared_ptr<vector<int>>;

shivec get_ivec()
{
    return std::make_shared<vector<int>>();
}

void fill_ivec(shivec ivec)
{
    std::istream_iterator<int> int_in(cin), eof;
    std::copy(int_in, eof, std::back_inserter(*ivec));
}

void print_ivec(shivec ivec)
{
    for (auto const& i : *ivec)
        cout << i << ",";
    cout << endl;
}
int main(int argc, char **argv)
{
    auto ivec = get_ivec();
    fill_ivec(ivec);
    print_ivec(ivec);
    return EXIT_SUCCESS;
}