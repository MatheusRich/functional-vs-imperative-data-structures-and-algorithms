#include "BinarySearchTree.hpp"

string assert(bool a, string msg) { return a ? msg : "Fail"; }
string to_s(int n) { return '"' + (to_string(n)) + '"'; }
bool is_negative(int n) { return n < 0; }
int sum(int a, int b) { return a + b; }

int main() {
  cout << "Creating a new binary search tree of integers" << endl;
  auto bst = BinarySearchTree<int>();
  cout << assert(bst.isEmpty(), "It is empty") << endl;
  cout << "Pushing 3, -1, -4 and 5" << endl;
  bst.push(3);
  bst.push(-1);
  bst.push(-4);
  bst.push(5);
  cout << assert(!bst.isEmpty(), "It is not empty") << endl;
  cout << "Depth is " << bst.depth() << endl;
  cout << "First negative number is " << *bst.find(is_negative) << endl;
}
