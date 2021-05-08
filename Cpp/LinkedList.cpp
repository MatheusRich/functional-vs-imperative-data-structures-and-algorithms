#include "LinkedList.h"

#include <sstream>

template <typename T>
ListNode<T>::ListNode(T value, ListNode<T>* next) {
  this->value = value;
  this->next = next;
}

template <typename T>
LinkedList<T>::LinkedList() {
  _head = nullptr;
}

template <typename T>
LinkedList<T>::LinkedList(ListNode<T>* head) {
  _head = head;
}

template <typename T>
T* LinkedList<T>::head() {
  return &_head->value;
}

template <typename T>
LinkedList<T> LinkedList<T>::tail() {
  if (!_head) return LinkedList();

  return LinkedList(_head->next);
}

template <typename T>
T* LinkedList<T>::last() {
  auto currentNode = _head;

  while (currentNode && currentNode->next) {
    currentNode = currentNode->next;
  }

  return currentNode ? &currentNode->value : nullptr;
}

template <typename T>
bool LinkedList<T>::isEmpty() {
  return length() == 0;
}

template <typename T>
int LinkedList<T>::length() {
  if (!head()) return 0;

  int length = 0;
  auto currentNode = _head;
  while (currentNode) {
    length++;
    currentNode = currentNode->next;
  }

  return length;
}

template <typename T>
void LinkedList<T>::push(T value) {
  auto tmp = new ListNode<T>(value, _head);
  _head = tmp;
}

template <typename T>
T* LinkedList<T>::index(int index) {
  if (index < 0 || index >= length()) return nullptr;

  int currentIndex = 0;
  auto currentNode = _head;

  while (currentIndex < index && currentNode) {
    currentNode = currentNode->next;
    currentIndex++;
  }

  return currentNode ? currentNode->value : nullptr;
}

template <typename T>
LinkedList<T> LinkedList<T>::reverse() {
  auto newList = LinkedList();
  auto currentNode = _head;

  while (currentNode) {
    newList.push(currentNode->value);
    currentNode = currentNode->next;
  }

  return newList;
}

template <typename T>
LinkedList<T> LinkedList<T>::mergeSort() {
  if (length() == 1) return *this;

  auto half = length() / 2;
  auto left = take(half);
  auto right = drop(half);

  return merge(left.mergeSort(), right.mergeSort());
}

template <typename T>
LinkedList<T> LinkedList<T>::merge(LinkedList<T> left, LinkedList<T> right) {
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
}

template <typename T>
LinkedList<T> LinkedList<T>::take(int n) {
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
}

template <typename T>
LinkedList<T> LinkedList<T>::drop(int n) {
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
}

template <typename T>
template <typename U>
LinkedList<U> LinkedList<T>::map(U (*fn)(T)) {
  if (!_head) return LinkedList<U>();

  auto newList = LinkedList<U>();

  auto currentNode = _head;
  while (currentNode) {
    newList.push(fn(currentNode->value));
    currentNode = currentNode->next;
  }

  return newList.reverse();
}

template <typename T>
LinkedList<T> LinkedList<T>::filter(bool (*fn)(T)) {
  if (!_head) return LinkedList<T>();

  auto newList = LinkedList<T>();

  auto currentNode = _head;
  while (currentNode) {
    if (fn(currentNode->value)) newList.push(currentNode->value);
    currentNode = currentNode->next;
  }

  return newList.reverse();
}

template <typename T>
T LinkedList<T>::reduce(T (*fn)(T, T), T acc) {
  if (!_head) return acc;

  auto currentNode = _head;
  while (currentNode) {
    acc = fn(currentNode->value, acc);
    currentNode = currentNode->next;
  }

  return acc;
}