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
using std::map;
using std::ostream;
using std::string;
using std::vector;

class Y;
class X
{
    Y *ypt;
};
class Y
{
    X x;
};

int main(int argc, char **argv)
{
    return EXIT_SUCCESS;
}