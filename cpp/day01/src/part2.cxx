#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <fstream>
#include <iostream>
#include <ranges>
#include "lib.hpp"

using namespace std;

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Invalid input parameters. Usage: part1 <file path>" << endl;
  }
  char *filename = argv[1];

  ifstream is(filename);
  vector<int> left;
  std::unordered_map<int, int> right;

  for (string line; getline(is, line);) {
    auto items = line_to_numbers(line);
    left.push_back(get<0>(items));
    if (right.find(get<1>(items)) == right.end()) {
      right[get<1>(items)] = 1;
    } else {
      right[get<1>(items)] += 1;
    }
  }
  int result = 0;
  for (auto item : left) {
    try {
      int freq = right.at(item);
      result += freq * item;
    } catch(out_of_range) {
    }
  }
  cout << result << endl;
  return 0;
}
