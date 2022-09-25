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

class Sales_data
{
public:
    Sales_data(string s, unsigned cnt, double price) : bookNo(s), units_sold(cnt), revenue(cnt * price)
    {
        cout << "cstr 1" << endl;
    }
    Sales_data() : Sales_data("", 0, 0)
    {
        cout << "cstr 2" << endl;
    }
    Sales_data(string s) : Sales_data(s, 0, 0)
    {
        cout << "cstr 3" << endl;
    }
    Sales_data(istream &is) : Sales_data()
    {
        read(is, *this);
    }
    istream &read(istream &is, Sales_data &item)
    {
        double price = 0;
        is >> item.bookNo >> item.units_sold >> price;
        item.revenue = price * item.units_sold;
        return is;
    }

private:
    string bookNo;
    unsigned units_sold = 0;
    double revenue = 0.0;
};

int main(int argc, char **argv)
{
    Sales_data("foo");
    Sales_data();
    Sales_data("abc", 1, 2);
    return EXIT_SUCCESS;
}