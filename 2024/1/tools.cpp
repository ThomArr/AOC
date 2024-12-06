#include "tools.hpp"

// Fonction pour partitionner le tableau
int Tools::partition(vector<int> &arr, int low, int high) {
  int pivot = arr[high]; // Pivot : le dernier élément
  int i = low - 1;       // Index pour les éléments plus petits que le pivot

  for (int j = low; j < high; j++) {
    if (arr[j] < pivot) {
      i++;
      swap(arr[i], arr[j]); // Échange des éléments
    }
  }
  swap(arr[i + 1], arr[high]); // Mettre le pivot à sa place
  return i + 1;
}

// Fonction principale Quicksort
void Tools::quicksort(vector<int> &arr, int low, int high) {
  if (low < high) {
    int pi = partition(arr, low, high); // Trouver l'indice du pivot

    quicksort(arr, low, pi - 1);  // Trier la partie gauche
    quicksort(arr, pi + 1, high); // Trier la partie droite
  }
}

// Compte le nombre d'occurence de target dans un vecteur trié.
int Tools::countOccurrences(vector<int> &arr, int target) {
  int size{arr.size()};

  // Trouve la première occurrence de l'élément
  int left{0}, right{size - 1}, first{-1}, first_indice_find{-1};
  ;
  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (arr[mid] == target) {
      if (first_indice_find == -1) {
        first_indice_find = mid;
      }
      first = mid;
      right = mid - 1; // Continue à chercher à gauche
    } else if (arr[mid] < target) {
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }

  // Si l'élément n'est pas présent
  if (first == -1) {
    return 0;
  }

  // Trouve la dernière occurrence de l'élément
  left = first_indice_find, right = size - 1;
  int last{-1};
  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (arr[mid] == target) {
      last = mid;
      left = mid + 1; // Continue à chercher à droite
    } else if (arr[mid] < target) {
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }

  last = last == -1 ? first : last;

  // Le nombre d'occurrences est la différence entre les indices
  return (last - first + 1);
}