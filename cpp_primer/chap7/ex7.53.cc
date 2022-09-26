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
using std::ostream;
using std::string;
using std::vector;

class Debug
{
public:
    constexpr Debug(bool b = true) : hw(b), io(b), other(b) {}
    constexpr bool any() { return wh || io || other }
    void set_io(bool b) { io = b; }

private:
    bool hw, io, other;
};
int main(int argc, char **argv)
{
    return EXIT_SUCCESS;
}