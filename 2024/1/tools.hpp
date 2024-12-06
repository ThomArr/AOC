#ifndef TOOLS
#define TOOLS

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Tools {
public:
  static int partition(vector<int> &arr, int low, int high);
  static void quicksort(vector<int> &arr, int low, int high);
  static int countOccurrences(vector<int> &arr, int target);
};

#endif