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
using std::string;
using std::vector;

struct Person
{
    string name;
    string address;
    Person(string name, string address) : name(name), address(address) {}
    string getName() const
    {
        return name;
    }
    string getAddress() const
    {
        return address;
    }
};

int main(int argc, char **argv)
{
    Person p = Person("santa", "north pole");
    cout << p.getName() << " lives at: " << p.getAddress() << endl;
    return EXIT_SUCCESS;
}