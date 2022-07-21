#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::vector;
int main()
{
    vector<string> svec;
    string item;
    while (cin >> item)
        svec.push_back(item);
    
    for (auto s : svec)
        cout << s << endl;
    return 0;
}