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

struct X
{
    int base, rem;
    X(int i, int j) : base(i), rem(base % j) {}
};
int main(int argc, char **argv)
{
    X(2, 3);
    return EXIT_SUCCESS;
}