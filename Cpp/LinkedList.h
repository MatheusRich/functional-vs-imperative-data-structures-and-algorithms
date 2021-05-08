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
public:
  ListNode<T> *_head;

  LinkedList();
  LinkedList(ListNode<T> *head);

  // retornar um ponteiro pro value do node
  T *head();
  LinkedList<T> tail();
  T *last();
  T *index(int index);
  LinkedList<T> reverse();
  template <typename U>
  LinkedList<U> map(U (*fn)(T));
  LinkedList<T> filter(bool (*fn)(T));
  T reduce(T (*fn)(T, T), T acc);
  bool isEmpty();
  int length();
  void push(T value);
  LinkedList<T> mergeSort();
  LinkedList<T> take(int n);
  LinkedList<T> drop(int n);
  LinkedList<T> merge(LinkedList<T> left, LinkedList<T> right);
};
