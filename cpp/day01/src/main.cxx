#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <fstream>
#include <iostream>
#include <ranges>

using namespace std;

tuple<int, int> line_to_numbers(string line);

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << "Invalid input parameters. Usage: part1 <file path>" << endl;
  }
  char *filename = argv[1];

  ifstream is(filename);
  vector<int> left, right;
  for (string line; getline(is, line);) {
    auto items = line_to_numbers(line);
    left.push_back(get<0>(items));
    right.push_back(get<1>(items));
  }
  sort(left.begin(), left.end());
  sort(right.begin(), right.end());
  int result = 0;
  for (size_t idx = 0; idx < left.size(); idx++) {
    result += abs(left[idx] - right[idx]);
  }
  cout << result << endl;
  return 0;
}

tuple<int, int> line_to_numbers(string line) {
  vector<string> results;
  boost::split(results, line, boost::is_any_of(" "));
  return {stoi(results.front()), stoi(results.back())};
}
