#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

vector<int> removeElement(vector<int> line, int indice) {
  line.erase(line.begin() + indice);
  return line;
}

int checkValid(vector<int> line) {
  long unsigned int size{line.size()};

  if (size > 2) {
    for (long unsigned int i{0}; i < size - 2; i++) {
      int val{line[i] - line[i + 1]};
      if (val * (line[i + 1] - line[i + 2]) >= 0) {
        if (val < -3 || 3 < val || val == 0) {
          return 1 + min(checkValid(removeElement(line, i)),
                         checkValid(removeElement(line, i + 1)));
        }
      } else {
        return 1 + min(min(checkValid(removeElement(line, i)),
                           checkValid(removeElement(line, i + 1))),
                       checkValid(removeElement(line, i + 2)));
      }
    }
    int val{line[size - 2] - line[size - 1]};
    if (val < -3 || 3 < val || val == 0) {
      return 1 + min(checkValid(removeElement(line, size - 2)),
                     checkValid(removeElement(line, size - 1)));
    }
  }
  if (size == 2) {
    int val{line[size - 2] - line[size - 1]};
    if (val < -3 || 3 < val || val == 0) {
      return 1;
    }
  }
  return 0;
}

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

    // Vérification si la ligne est safe

    sum = checkValid(line) >= 2 ? sum : sum + 1;

    line.clear();
  }

  cout << "Sum = " << sum << endl;

  return 0;
}
