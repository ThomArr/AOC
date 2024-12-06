#include "tools.hpp"

int main(int argc, char *argv[]) {
  vector<int> left{};
  vector<int> right{};

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

  int i{0};
  string x;

  // Lecture des valeurs du fichier
  while (inFile >> x) {
    try {
      int value{stoi(x)};
      if (i == 0) {
        left.push_back(value);
      } else {
        right.push_back(value);
      }
      i = (i + 1) % 2;
    } catch (invalid_argument &) {
      cout << "Invalid number in file: " << x << endl;
      return 1;
    }
  }

  // Vérifier si le fichier a un nombre impair d'éléments
  if (i != 0) {
    cout << "File contains an odd number of inputs." << endl;
    return 1;
  }

  // Calculer la somme
  int sum{0};

  Tools::quicksort(right, 0, right.size() - 1);

  while (!left.empty()) {
    // Récupérer les éléments
    int leftValue{left.back()};
    left.pop_back();

    // Ajouter à la somme
    int value{Tools::countOccurrences(right, leftValue)};
    sum += value * leftValue;
  }
  cout << endl;

  inFile.close();

  cout << "Sum = " << sum << endl;
  return 0;
}
