#include "LinkedList.cpp"

string toString(bool a) { return a == true ? "true" : "false"; }

int main() {
  auto l = LinkedList<int>();
  cout << "is empty: " << toString(l.isEmpty()) << endl;
  l.push(5);
  l.push(2);
  l.push(1);
  l.push(7);

  cout << l.index(0)->value << endl;
  cout << l.index(1)->value << endl;
  cout << l.index(3)->value << endl;

  // cout << l.toString() << endl;
}