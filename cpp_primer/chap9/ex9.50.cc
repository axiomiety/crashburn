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
using std::begin;
using std::cerr;
using std::cin;
using std::cout;
using std::deque;
using std::end;
using std::endl;
using std::forward_list;
using std::getline;
using std::ifstream;
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

int main(int argc, char **argv)
{
    vector<string> svec = {"1", "2", "3", "4", "5"};
    int total = 0;
    for (const auto &item : svec)
    {
        total += stoi(item);
    }
    cout << total << endl;

    vector<string> sfvec = {"1.1", "2.2", "3.3", "4.4", "5.5"};
    float ftotal = 0;
    for (const auto &item : sfvec)
    {
        ftotal += stof(item);
    }
    cout << ftotal << endl;
    return EXIT_SUCCESS;
}