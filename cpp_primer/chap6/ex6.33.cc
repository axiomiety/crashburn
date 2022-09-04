#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
using std::begin;
using std::cin;
using std::cout;
using std::end;
using std::endl;
using std::initializer_list;
using std::map;
using std::string;
using std::vector;

void recursive_print(vector<int> *vp, int idx)
{
    if (idx < vp->size())
    {
        cout << (*vp)[idx] << endl;
        recursive_print(vp, ++idx);
    }
}

int main(int argc, char **argv)
{
    vector<int> vint{1, 2, 3, 4};
    recursive_print(&vint, 0);
    return EXIT_SUCCESS;
}