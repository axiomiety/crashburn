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
#include <sstream>

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
using std::ostream;
using std::ostringstream;
using std::string;
using std::vector;
using std::pair;
using namespace std::placeholders;

int main(int argc, char **argv)
{
    vector<pair<string, int>> pvec;
    bool toggle = false;
    string token, key;
    while (cin >> token)
    {
        if (toggle)
        {
            pvec.emplace_back(key, std::stoi(token));
            toggle = false;
        }
        else {
            key = token;
            toggle = true;
        }
    }
    // increment each key by 1
    for (auto& p: pvec)
        p.second += 1;
    for (const auto& p : pvec)
        cout << p.first << " -> " << p.second << endl;

    return EXIT_SUCCESS;
}