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
  bool enable{true};

  string lineContent;
  // Lecture ligne par ligne
  while (getline(inFile, lineContent)) {
    istringstream lineStream(lineContent);

    string x;
    while (lineStream >> x) {
      int size{x.length()};
      for (int i = 0; i <= size - 3; ++i) {
        if (x.substr(i, 4).compare("do()") == 0) {
          enable = true;
          i = i+3;
          continue;
        }
        if (x.substr(i, 7).compare("don't()") == 0) {
          enable = false;
          i = i+6;
          continue;
        }
        if (!enable){
            continue;
        }
        if (x.substr(i, 4).compare("mul(") != 0) {
          continue;
        }
        int j{4};
        int fst{0};
        vector<int> nbr;
        while ('0' <= x[i + j] && x[i + j] <= '9') {
          fst += x[i + j] - '0';
          fst *= 10;
          j++;
        }
        fst /= 10;
        if (x[i + j] != ',') {
          continue;
        }
        j++;
        int scd{0};
        while ('0' <= x[i + j] && x[i + j] <= '9') {
          scd += x[i + j] - '0';
          scd *= 10;
          j++;
        }
        scd /= 10;

        if (x[i + j] != ')') {
          continue;
        }

        i = i+j;

        sum += fst * scd;
      }
    }

    line.clear();
  }

  cout << "Sum = " << sum << endl;

  return 0;
}
