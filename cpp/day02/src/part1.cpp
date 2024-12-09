#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

bool valid(string);
vector<string> split(const string &str, char delimiter);

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Invalid argument. Usage: part1 <file path>";
    return 1;
  }

  char *filename = argv[1];

  ifstream is(filename);
  int total;
  for (string line; getline(is, line);) {
    if (valid(line)) {
      total += 1;
    }
  }
  cout << total << endl;
  return 0;
}

bool valid(string line) {
  auto tokens = split(line, ' ');
  vector<int> results(tokens.size());
  transform(tokens.begin(), tokens.end(), results.begin(),
            [](string el) { return stoi(el); });
  for (auto& item : results)
    cout << item << ' ';
  return false;
}

vector<string> split(const string &str, char delimiter) {
  vector<string> tokens;
  stringstream ss;
  string token;
  while (getline(ss, token, delimiter)) {
    tokens.push_back(token);
  }
  return tokens;
}
