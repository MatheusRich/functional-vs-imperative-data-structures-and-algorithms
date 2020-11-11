#include "LinkedList.cpp"

string boolToString(bool a) { return a == true ? "true" : "false"; }

int main() {
  auto l = LinkedList<int>();
  cout << "Empty? " << boolToString(l.isEmpty()) << endl;
  cout << "Pushing 5";
  l.push(5);
  cout << ", 2";
  l.push(2);
  cout << ", 1";
  l.push(1);
  cout << " and 7" << endl;
  l.push(7);

  cout << "String Representation: " << l.toString() << endl;
  cout << "Length: " << l.length() << endl;
  cout << "Head: " << l.head()->value << endl;
  cout << "Tail (first): " << l.tail()->value << endl;

  // cout << l.toString() << endl;
}