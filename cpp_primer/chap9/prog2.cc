#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
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
    vector<int> ivec = {1, 2, 3};
    ivec.front() = 42;
    cout << "front: " << ivec.front() << endl;
    auto f = ivec.front(); // we got a copy!
    f = 66;
    cout << "front: " << ivec.front() << endl;
    auto &g = ivec.front(); // we got a reference this time
    g = 88;
    cout << "front: " << ivec.front() << endl;

    return EXIT_SUCCESS;
}