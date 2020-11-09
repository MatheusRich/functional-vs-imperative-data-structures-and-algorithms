#include <iostream>

using namespace std;

template <typename T>
class ListNode {
public:
  T value;
  ListNode<T> *next;

  ListNode(T value, ListNode<T> *next);
};

template <typename T>
class LinkedList {
private:
  ListNode<T> *head;
  int size;

public:
  LinkedList();

  ListNode<T> *getHead();
  ListNode<T> *index(int index);
  bool isEmpty();
  void push(T value);
  string toString();
};
