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
    int arr[] = {1,2,3,4,5,6,7,8,9,0};
    vector<int> ivec(begin(arr),end(arr));
    for (auto i : ivec)
        cout << i << endl;
    return 0;
}