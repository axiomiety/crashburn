#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;
using std::vector;
using std::string;

int main()
{
    cout << "int " << sizeof (int) << endl;
    cout << "uint " << sizeof (uint) << endl;
    cout << "char " << sizeof (char) << endl;
    cout << "string " << sizeof string("hello") << endl;
    // this is the size of the vector object itself - not its constituents!
    cout << "vector<int>(10) " << sizeof vector<int>(20,0) << endl;
}