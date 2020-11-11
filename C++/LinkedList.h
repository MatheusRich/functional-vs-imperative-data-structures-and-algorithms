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
  ListNode<T> *_head;
  int size;

public:
  LinkedList();

  ListNode<T> *head();
  ListNode<T> *tail();
  ListNode<T> *last();
  ListNode<T> *index(int index);
  bool isEmpty();
  int length();
  void push(T value);
  string toString();
};
