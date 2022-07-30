#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;
using std::vector;

int main()
{
    vector<int> ivec = {0,1,2,3,4,5,6,7,8,9};
    for (auto &i : ivec)
        i = (i % 2) ? i : i*2;
    
    for (const auto &i : ivec)
        cout << i << " ";
    cout << endl;
    return 0;
}