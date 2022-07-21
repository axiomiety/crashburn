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
    vector<string> svec = {"a","b","c"};
    vector<string> svec2(svec);
    svec[0] = "z";
    cout << svec2[0] << endl;
    return 0;
}