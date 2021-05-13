#include <iostream>

using namespace std;

template <typename T>
class ListNode {
public:
  T value;
  ListNode<T> *next;

  ListNode(T value, ListNode<T> *next) {
    this->value = value;
    this->next = next;
  };
};

template <typename T>
class LinkedList {
public:
  ListNode<T> *_head;

  LinkedList() { _head = nullptr; };
  LinkedList(ListNode<T> *head) { _head = head; };

  T *head() { return &_head->value; };
  LinkedList<T> tail() {
    if (!_head) return LinkedList();

    return LinkedList(_head->next);
  };

  T *last() {
    auto currentNode = _head;

    while (currentNode && currentNode->next) {
      currentNode = currentNode->next;
    }

    return currentNode ? &currentNode->value : nullptr;
  };

  T *index(int index) {
    if (index < 0 || index >= length()) return nullptr;

    int currentIndex = 0;
    auto currentNode = _head;

    while (currentIndex < index && currentNode) {
      currentNode = currentNode->next;
      currentIndex++;
    }

    return currentNode ? currentNode->value : nullptr;
  };

  LinkedList<T> reverse() {
    auto newList = LinkedList();
    auto currentNode = _head;

    while (currentNode) {
      newList.push(currentNode->value);
      currentNode = currentNode->next;
    }

    return newList;
  };

  template <typename U>
  LinkedList<U> map(U (*fn)(T)) {
    if (!_head) return LinkedList<U>();

    auto newList = LinkedList<U>();

    auto currentNode = _head;
    while (currentNode) {
      newList.push(fn(currentNode->value));
      currentNode = currentNode->next;
    }

    return newList.reverse();
  };

  LinkedList<T> filter(bool (*fn)(T)) {
    if (!_head) return LinkedList<T>();

    auto newList = LinkedList<T>();

    auto currentNode = _head;
    while (currentNode) {
      if (fn(currentNode->value)) newList.push(currentNode->value);
      currentNode = currentNode->next;
    }

    return newList.reverse();
  };

  T reduce(T (*fn)(T, T), T acc) {
    if (!_head) return acc;

    auto currentNode = _head;
    while (currentNode) {
      acc = fn(currentNode->value, acc);
      currentNode = currentNode->next;
    }

    return acc;
  };

  bool isEmpty() { return length() == 0; };

  int length() {
    if (!head()) return 0;

    int length = 0;
    auto currentNode = _head;
    while (currentNode) {
      length++;
      currentNode = currentNode->next;
    }

    return length;
  };

  LinkedList<T> *push(T value) {
    auto tmp = new ListNode<T>(value, _head);
    _head = tmp;

    return this;
  };

  LinkedList<T> mergeSort() {
    if (length() == 1) return *this;

    auto half = length() / 2;
    auto left = take(half);
    auto right = drop(half);

    return merge(left.mergeSort(), right.mergeSort());
  };

  LinkedList<T> take(int n) {
    auto newList = LinkedList();
    if (n == 0) return newList;

    auto node = _head;
    if (!node) return newList;

    for (int i = 0; i < n; i++) {
      newList.push(node->value);
      node = node->next;

      if (!node) break;
    }

    return newList.reverse();
  };

  LinkedList<T> drop(int n) {
    auto newList = LinkedList();
    if (n == length()) return newList;

    auto node = _head;
    if (!node) return newList;

    for (int i = 0; i < n; i++) {
      node = node->next;

      if (!node) break;
    }

    while (node) {
      newList.push(node->value);
      node = node->next;
    }

    return newList.reverse();
  };

  LinkedList<T> merge(LinkedList<T> left, LinkedList<T> right) {
    auto leftNode = left._head;
    auto rightNode = right._head;
    auto newList = LinkedList();

    while (leftNode && rightNode) {
      if (leftNode->value < rightNode->value) {
        newList.push(leftNode->value);
        leftNode = leftNode->next;
      } else {
        newList.push(rightNode->value);
        rightNode = rightNode->next;
      }
    }

    while (leftNode) {
      newList.push(leftNode->value);

      leftNode = leftNode->next;
    }

    while (rightNode) {
      newList.push(rightNode->value);

      rightNode = rightNode->next;
    }

    return newList.reverse();
  };
};
