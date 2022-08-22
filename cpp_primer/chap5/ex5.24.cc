#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::map;
using std::string;
using std::vector;

int main()
{
    int a, b;
    cout << "Enter 2 integers: ";
    while (cin >> a >> b)
    {
        try
        {
            if (b == 0)
                throw std::invalid_argument("b cannot be 0");
            cout << "a/b: " << a / b << endl;
        }
        catch (std::invalid_argument exc)
        {
            cout << exc.what() << " Try again? Enter y or n: ";
            char c;
            cin >> c;
            if (!cin || c == 'n')
                break;
            cout << "Enter 2 integers: ";
        }
    }
    return 0;
}