#include <iostream>
#include <string>
#include <vector>
using std::cin;
using std::cout;
using std::endl;
using std::begin;
using std::end;

bool is_array_same(int *start1, int *end1, int *start2, int *end2) {
    // first we check the number of elements
    if (end1-start1 == end2-start2) {
        while (end1 != start1) {
            if (*start1 != *start2)
                return false;
            ++start1;
            ++start2;
        }
        return true;
    }

    return false;
}

int main()
{
    int arr1[] = {0,1};
    int arr2[] = {0,1,2};
    cout << is_array_same(begin(arr1), end(arr1), begin(arr2), end(arr2)) << endl;
    cout << is_array_same(begin(arr1), end(arr1), begin(arr1), end(arr1)) << endl;
    return 0;
}