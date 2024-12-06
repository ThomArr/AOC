#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

int main(int argc, char *argv[]) {
  vector<int> line{};

  // Vérification des arguments
  if (argc < 2) {
    cout << "Usage: " << argv[0] << " <filename>" << endl;
    return 1;
  }

  ifstream inFile{argv[1]};
  if (!inFile) {
    cout << "Unable to open file" << endl;
    return 1;
  }

  int sum{0};

  string lineContent;
  // Lecture ligne par ligne
  while (getline(inFile, lineContent)) {
    istringstream lineStream(lineContent);

    string x;
    while (lineStream >> x) {
      try {
        int value{stoi(x)};
        line.push_back(value);
      } catch (invalid_argument &) {
        cout << "Invalid number in file: " << x << endl;
        return 1;
      }
    }

    long unsigned int size{line.size()};

    // Vérification si la ligne est safe
    bool safe{true};

    if (size > 2) {
      for (long unsigned int i{0}; i < size - 2 && safe; i++) {
        int val{line[i] - line[i + 1]};
        if (val * (line[i + 1] - line[i + 2]) > 0) {
          if (val < -3 || 3 < val) {
            safe = false;
          }
        } else {
          safe = false;
        }
      }
      if (safe) {
        int val{line[size - 2] - line[size - 1]};
        val = val < 0 ? -val : val;
        safe = val > 3 || val == 0 ? false : true;
      }
    }
    if (size == 2) {
      int val{line[0] - line[1]};
      val = val < 0 ? -val : val;
      safe = val > 3 || val == 0 ? false : true;
    }

    sum = safe ? sum + 1 : sum;

    line.clear();
  }

  cout << "Sum = " << sum << endl;

  return 0;
}
