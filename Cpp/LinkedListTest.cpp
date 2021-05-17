#include <sstream>

#include "LinkedList.hpp"

string assert(bool a, string msg) { return a ? msg : "Fail"; }
string to_s(int n) { return '"' + (to_string(n)) + '"'; }
bool is_odd(int n) { return n % 2 == 1; }
int sum(int a, int b) { return a + b; }

template <typename T>
string listToString(LinkedList<T> list) {
  stringstream ss;

  auto current = list._head;
  while (current) {
    ss << current->value << " -> ";
    current = current->next;
  }
  ss << "X";

  return ss.str();
}

int main() {
  cout << "Creating a new linked list of integers" << endl;
  auto l = LinkedList<int>();
  auto l2 = LinkedList<int>();
  cout << assert(l.isEmpty(), "It is empty") << endl;
  cout << "Pushing 3, 1, 4, 1, and 5" << endl;
  l.push(3)->push(1)->push(4)->push(1)->push(5);
  l2.push(3)->push(1)->push(4)->push(1)->push(5);
  cout << assert(!l.isEmpty(), "It is not empty") << endl;
  cout << "Length is " << l.length() << endl;
  cout << "List is " << listToString(l) << endl;
  cout << "Head is " << *l.head() << endl;
  cout << "Tail is " << listToString(l.tail()) << endl;
  cout << "Tail length is " << l.tail().length() << endl;
  cout << "Last is " << *l.last() << endl;
  cout << "Reversed list is " << listToString(l.reverse()) << endl;
  cout << "List as string is " << listToString(l.map(to_s)) << endl;
  cout << "List without even numbers is " << listToString(*l2.filter(is_odd)) << endl;
  cout << "Sum of all list elements is " << l.reduce(sum, 0) << endl;
  cout << "Sorted list is " << listToString(l.mergeSort()) << endl;
}
