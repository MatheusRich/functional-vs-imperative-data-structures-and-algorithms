#include "BinarySearchTree.h"
#define ERROR -9999999

template <typename T>
TreeNode<T>::TreeNode(T value) {
  this->_value = value;
  this->_left = nullptr;
  this->_right = nullptr;
}

template <typename T>
int TreeNode<T>::depth() {
  int leftDepth = _left == nullptr ? 0 : _left->depth();
  int rightDepth = _right == nullptr ? 0 : _right->depth();

  return 1 + max(leftDepth, rightDepth);
}

template <typename T>
void TreeNode<T>::push(T value) {
  if (value < _value) {
    if (_left == nullptr)
      _left = new TreeNode(value);
    else
      _left->push(value);
  } else if (value > _value) {
    if (_right == nullptr)
      _right = new TreeNode(value);
    else
      _right->push(value);
  }
}

template <typename T>
T TreeNode<T>::find(bool (*fn)(T)) {
  if (fn(_value)) return _value;

  int leftFind = _left ? _left->find(fn) : ERROR;
  if (leftFind) return leftFind;

  int rightFind;
  if (_right) rightFind = _right->find(fn);
  if (rightFind) return rightFind;

  return ERROR;
}

template <typename T>
BinarySearchTree<T>::BinarySearchTree() {
  _root = nullptr;
}

template <typename T>
bool BinarySearchTree<T>::isEmpty() {
  return _root == nullptr;
}

template <typename T>
int BinarySearchTree<T>::depth() {
  if (_root == nullptr) return 0;

  return _root->depth();
}

template <typename T>
void BinarySearchTree<T>::push(T value) {
  if (_root == nullptr)
    _root = new TreeNode<T>(value);
  else
    _root->push(value);
}

template <typename T>
T BinarySearchTree<T>::find(bool (*fn)(T)) {
  if (_root == nullptr) return ERROR;

  return _root->find(fn);
}
