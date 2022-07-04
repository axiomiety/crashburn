#include <iostream>

int main()
{
    int sum=0,val=50,max=100;
    while (val <= max) {
        sum += val;
        ++val;
    }
    std::cout << "Sum of " << val << " to " <<max << " inclusive is " << sum << std::endl;
    return 0;
}