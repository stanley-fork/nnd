#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <set>
#include <deque>
#include <queue>
#include <memory>
#include <iostream>
#include <string>
#include <atomic>
#include <utility>
using namespace std;

int main() {
    vector<int> v = {10, 20, 30};
    unordered_map<string, int> um = {{"asd", 42}, {"qwe", 13}};
    unordered_set<string> us = {"nya", "kawaii"};
    map<int, string> m = {{100, "pika"}, {200, "chu"}};
    set<int> s = {10, 20, 30, 40};
    deque<int> d = {10, 20, 30};
    unique_ptr<vector<int>> pv(new vector<int>{10, 20});
    shared_ptr<set<int>> sps(new set<int>{100, 200, 300});
    atomic<int> a = {42};
    pair<int, string> p = {12, "hi"};
    priority_queue<int> q; q.push(13); q.push(42);

    int h = 0;
    for(auto x:v) h+=x;
    for(auto&[k,x]:um) h+=(int)k.size()+x;
    for(auto&x:s) h+=x;
    for(auto&[k,x]:m) h+=k+(int)x.size();
    for(auto x:s) h+=x;
    for(auto x:d) h+=x;
    for(auto x:*pv) h+=x;
    for(auto x:*sps) h+=x;
    h+=a.load();
    h+=p.first+(int)p.second.size();
    while(!q.empty()){h+=q.top();q.pop();}
    cout<<h<<endl;
}
