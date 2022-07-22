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
    vector<int> v{1,2,3,4,5,6,7,8,9};
    vector<int> v2(v);
    for (auto &i: v)
        i *= i;
    for (auto i = 0; i<v2.size(); ++i)
        v2[i] *= v2[i];
    for (auto i :v)
        cout << i << " ";
    cout << endl;
    for (auto i :v2)
        cout << i << " ";
    cout << endl;
    return 0;
}