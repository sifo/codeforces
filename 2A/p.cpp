#include <iostream>
#include <cstdio>
#include <map>
#include <vector>
#include <utility>
#include <algorithm>
using namespace std;

template <typename T> 
bool contains(std::vector<T>& v, const T& e) {
  return std::find(v.begin(), v.end(), e) != v.end();
}

int main() {
  int n;
  int points;
  string name;
  vector<string> winners;
  map<string, int> players;
  vector<pair<string, int>> scores;

  cin >> n;
  for(int i = 0; i < n; i++) {
    cin >> name >> points;
    scores.push_back(make_pair(name, points));
    players[name] += points;
    if(winners.size() == 0) {
      winners.push_back(name);
    } else if(players[name] > players[winners.at(0)]) {
      winners.clear();
      winners.push_back(name);
    } else if(players[name] == players[winners.at(0)] && !contains(winners, name)) {
      winners.push_back(name);
    } else if(points < 0 && contains(winners, name)) {
      for(auto const& p : players) {
        if(p.first != name) {
          if(p.second > players[name]) {
            winners.clear();
            winners.push_back(p.first);
            name = p.first;
          } else if(p.second == players[name] && !contains(winners, p.first)) {
            winners.push_back(p.first);
          }
        }
      }
    }
  }

  if(winners.size() == 1) {
    cout << winners.at(0) << endl;
    return 0;
  }

  int max = players[winners.at(0)];
  players.clear();

  for(auto s : scores) {
    name = s.first;
    if(contains(winners, name)) {
      players[name] += s.second;
      if(players[name] >= max) {
        cout << name << endl;
        return 0;
      }
    }
  }

  return 0;
}
