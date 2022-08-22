#include <iostream>
#include <string>
#include <vector>
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
    string s, prev;
    bool dupe = false;
    while (cin >> s)
    {
        if (prev.empty())
        {
            prev = s;
        }
        else
        {
            if (prev == s)
            {
                cout << "repeated word was: " << prev << endl;
                dupe = true;
                break;
            }
            else
                prev = s;
        }
    }
    if (!dupe)
        cout << "no repeated word found" << endl;
    return 0;
}