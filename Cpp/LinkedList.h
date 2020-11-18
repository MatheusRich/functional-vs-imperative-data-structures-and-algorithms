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
  int _size;

public:
  LinkedList();
  LinkedList(ListNode<T> *head, int size);

  ListNode<T> *head();
  LinkedList<T> tail();
  ListNode<T> *last();
  ListNode<T> *index(int index);
  LinkedList<T> reverse();
  template <typename U>
  LinkedList<U> map(U (*fn)(T));
  LinkedList<T> filter(bool (*fn)(T));
  T reduce(T (*fn)(T, T), T acc);
  bool isEmpty();
  int length();
  void push(T value);
  string toString();
};
