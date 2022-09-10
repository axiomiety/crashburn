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

struct Person
{
    string name;
    string address;
    Person(char *name);
    Person(string name);
    Person(string name, string address) : name(name), address(address) {}
    string getName() const
    {
        return name;
    }
    string getAddress() const
    {
        return address;
    }
    ostream &print(ostream &os);
};

ostream &Person::print(ostream &os)
{
    os << name << " lives at: " << address;
    return os;
}

// note that if the arg is called `name`, this will
// mask the local definition of the `name` member variable!
Person::Person(string n)
{
    cout << "string constructor" << endl;
    name = n;
}

Person::Person(char *name) : Person(string(name), "N/A")
{
    cout << "char* constructor" << endl;
}

int main(int argc, char **argv)
{
    Person p = Person("santa", "north pole");
    p.print(cout) << endl;
    Person p2 = Person(string("Easter bunny"));
    p2.print(cout) << endl;
    Person p3 = Person(string("tooth fairy").c_str());
    p3.print(cout) << endl;
    return EXIT_SUCCESS;
}