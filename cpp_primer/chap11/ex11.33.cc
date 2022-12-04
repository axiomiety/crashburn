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

map<string, string> build_map(ifstream &map_file)
{
    map<string, string> trans_map;
    string key, value;
    while (map_file >> key && getline(map_file, value))
    {
        if (value.size() > 1)
            trans_map[key] = value.substr(1);
        else
            throw std::runtime_error("no rule for " + key);
    }

    return trans_map;
}
const string& transform(const string& s, const map<string, string> &m)
{
    auto map_it = m.find(s);
    if (map_it != m.end())
        return map_it->second;
    return s;
}

void word_transform(ifstream &map_file, ifstream& input)
{
    auto trans_map = build_map(map_file);
    string text;
    while (getline(input, text))
    {
        istringstream stream(text);
        string word;
        bool first_word = true;
        while (stream >> word)
        {
            if (first_word)
                first_word = false;
            else
                cout << " ";
            
            cout << transform(word, trans_map);
        }
        cout << endl;
    }
}

int main(int argc, char **argv)
{
    ifstream map_file(argv[1]);
    ifstream input(argv[2]);
    word_transform(map_file, input);
    return EXIT_SUCCESS;
}