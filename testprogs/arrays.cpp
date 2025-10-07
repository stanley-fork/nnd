#include <iostream>
#include <array>
#include <string>
using namespace std;

int main() {
    volatile int a[5] = {1, 2, 3, 4, 5};
    volatile array<string, 4> b = {"a", "b", "c", "d"};
    volatile long c[3][2] = {{1, 2}, {3, 4}, {5, 6}};
    volatile int len = 10;
    volatile long d[len];
    d[0] = 13;
    d[5] = 7;
    volatile int e[4][3][2] = {{{1, 2}, {3, 4}, {5, 6}}, {{7, 8}, {9, 10}, {11, 12}}, {{13, 14}, {15, 16}, {17, 18}}, {{19, 20}, {21, 22}, {23, 24}}};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 3; ++j)
            e[i][j][1] = e[i][j][1] + e[i][j][0] * 10;
    cout<<"hi"<<endl;
}
