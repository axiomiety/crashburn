#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;

int main()
{
    int arr[] = {0,1,2,3,4,5,6,7,8,9};
    int *p = arr;
    int *e = &arr[10];
    for (int *b = arr; b != e; ++b)
        cout << *b << endl;

    e = end(arr);
    for (int *b = begin(arr); b != e; ++b)
        cout << *b << endl;
    return 0;
}