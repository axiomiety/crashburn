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
    list<int> ilist = {1, 2, 3,4,5,6,7,9};
    deque<int> even, odd;
    for (const auto &i : ilist)
    {
        if (i%2 == 0)
            even.push_back(i);
        else
            odd.push_back(i);
    }

    cout << "even: " ;
    auto it_even = even.cbegin();
    while (it_even != even.cend())
        cout << *(it_even++) << " ";
    cout << endl;

    cout << "odd: " ;
    auto it_odd = odd.cbegin();
    while (it_odd != odd.cend())
        cout << *(it_odd++) << " ";
    cout << endl;

    return EXIT_SUCCESS;
}