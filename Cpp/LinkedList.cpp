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
ListNode<T>* LinkedList<T>::head() {
  return _head;
}

template <typename T>
LinkedList<T> LinkedList<T>::tail() {
  if (_head == nullptr) return LinkedList();

  return LinkedList(_head->next);
}

template <typename T>
ListNode<T>* LinkedList<T>::last() {
  auto currentNode = head();

  while (currentNode != nullptr && currentNode->next != nullptr) {
    currentNode = currentNode->next;
  }

  return currentNode;
}

template <typename T>
bool LinkedList<T>::isEmpty() {
  return length() == 0;
}

template <typename T>
int LinkedList<T>::length() {
  if (!head()) return 0;

  int length = 0;
  auto currentNode = head();
  while (currentNode != nullptr) {
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
ListNode<T>* LinkedList<T>::index(int index) {
  if (index < 0 || index >= length()) return nullptr;

  int currentIndex = 0;
  auto currentNode = head();

  while (currentIndex < index && currentNode) {
    currentNode = currentNode->next;
    currentIndex++;
  }

  return currentNode;
}

template <typename T>
LinkedList<T> LinkedList<T>::reverse() {
  auto newList = LinkedList();
  auto currentNode = head();

  while (currentNode != nullptr) {
    newList.push(currentNode->value);
    currentNode = currentNode->next;
  }

  return newList;
}

template <typename T>
string LinkedList<T>::toString() {
  stringstream ss;

  auto current = head();
  while (current != nullptr) {
    ss << current->value << " -> ";
    current = current->next;
  }
  ss << "X";

  return ss.str();
}

template <typename T>
template <typename U>
LinkedList<U> LinkedList<T>::map(U (*fn)(T)) {
  if (_head == nullptr) return LinkedList<U>();

  auto newList = LinkedList<U>();

  auto currentNode = head();
  while (currentNode != nullptr) {
    newList.push(fn(currentNode->value));
    currentNode = currentNode->next;
  }

  return newList.reverse();
}

template <typename T>
LinkedList<T> LinkedList<T>::filter(bool (*fn)(T)) {
  if (_head == nullptr) return LinkedList<T>();

  auto newList = LinkedList<T>();

  auto currentNode = head();
  while (currentNode != nullptr) {
    if (fn(currentNode->value)) newList.push(currentNode->value);
    currentNode = currentNode->next;
  }

  return newList.reverse();
}

template <typename T>
T LinkedList<T>::reduce(T (*fn)(T, T), T acc) {
  if (_head == nullptr) return acc;

  auto currentNode = head();
  while (currentNode != nullptr) {
    acc = fn(currentNode->value, acc);
    currentNode = currentNode->next;
  }

  return acc;
}