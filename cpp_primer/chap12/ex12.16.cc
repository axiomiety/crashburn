#include <iostream>
#include <fstream>
#include <deque>
#include <list>
#include <sstream>
#include <string>
#include <vector>
#include <forward_list>
#include <stdexcept>
#include <map>
#include <initializer_list>
#include <cstdlib>
#include <memory>
#include <algorithm>
#include <iterator>
#include <functional>
#include <map>
#include <set>
#include <cassert>

using namespace std::placeholders;
using namespace std;

void custom_delete(int* p)
{
    cout << "deleting " <<  *p << endl;
    delete(p);
}
int main(int argc, char **argv)
{
    unique_ptr<int, decltype(custom_delete)*> up {new int(3), custom_delete};
    //auto up2 = up;
    return EXIT_SUCCESS;
}