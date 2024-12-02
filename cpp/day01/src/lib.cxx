#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include "lib.hpp"

std::tuple<int, int> line_to_numbers(std::string line) {
  std::vector<std::string> results;
  boost::split(results, line, boost::is_any_of(" "));
  return {stoi(results.front()), stoi(results.back())};
}
