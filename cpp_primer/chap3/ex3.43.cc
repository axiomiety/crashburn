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
    int ia[3][4] = {{0,1,2,3},{4,5,6,7},{8,9,10,11}};

    cout << "auto" << endl;
    for (auto &row : ia) {
        for (auto col : row) {
            cout << col << " ";
        }
        cout << endl;
    }
    cout << "subscript" << endl;
    for (size_t row = 0; row < 3; ++row) {
        for (size_t col = 0; col < 4; ++col) {
            cout << ia[row][col] << " ";
        }
        cout << endl;
    }
    cout << "ptr" << endl;
    for (int (*p)[4] = begin(ia); p != end(ia); ++p) {
        for (int *q = begin(*p); q != end(*p); ++q) {
            cout << *q << " ";
        }
        cout << endl;
    }
    return 0;
}