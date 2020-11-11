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
  size = 0;
}

template <typename T>
ListNode<T>* LinkedList<T>::head() {
  return _head;
}

template <typename T>
ListNode<T>* LinkedList<T>::tail() {
  if (_head == nullptr) return nullptr;

  return _head->next;
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
  return _head == nullptr;
}

template <typename T>
int LinkedList<T>::length() {
  return size;
}

template <typename T>
void LinkedList<T>::push(T value) {
  auto tmp = new ListNode<T>(value, _head);
  _head = tmp;
  size++;
}

template <typename T>
ListNode<T>* LinkedList<T>::index(int index) {
  if (index < 0 || index >= size) return nullptr;

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

  while (currentNode) {
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
