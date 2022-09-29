#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::initializer_list;
using std::istream;
using std::map;
using std::string;
using std::vector;

istream &myread(istream &is)
{
    string s;
    while (is >> s)
    {
        cout << s << endl;
    }
    is.clear();
    return is;
}

int main(int argc, char **argv)
{
    myread(cin);
    cout << cin.rdstate() << endl;
    return EXIT_SUCCESS;
}