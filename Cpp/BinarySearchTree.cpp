#include "BinarySearchTree.h"

template <typename T>
TreeNode<T>::TreeNode(T value) {
  this->_value = value;
  this->_left = nullptr;
  this->_right = nullptr;
}

template <typename T>
int TreeNode<T>::depth() {
  int leftDepth = !_left ? 0 : _left->depth();
  int rightDepth = !_right ? 0 : _right->depth();

  return 1 + max(leftDepth, rightDepth);
}

template <typename T>
void TreeNode<T>::push(T value) {
  if (value < _value) {
    if (!_left)
      _left = new TreeNode(value);
    else
      _left->push(value);
  } else if (value > _value) {
    if (!_right)
      _right = new TreeNode(value);
    else
      _right->push(value);
  }
}

template <typename T>
bool TreeNode<T>::contains(T value) {
  if (value < _value) {
    if (!_left) return false;
    return _left->contains(value);
  } else if (value > _value) {
    if (!_right) return false;
    return _right->contains(value);
  }

  return true;
}

template <typename T>
template <typename U>
TreeNode<U>* TreeNode<T>::map(U (*fn)(T)) {
  auto newNode = new TreeNode(fn(_value));

  if (_left) newNode->_left = _left->map(fn);
  if (_right) newNode->_right = _right->map(fn);

  return newNode;
}

template <typename T>
T* TreeNode<T>::find(bool (*fn)(T)) {
  if (fn(_value)) return &_value;

  auto leftFind = _left ? _left->find(fn) : nullptr;
  if (leftFind) return leftFind;

  auto rightFind = _right ? _right->find(fn) : nullptr;
  if (rightFind) return rightFind;

  return nullptr;
}

template <typename T>
BinarySearchTree<T>::BinarySearchTree() {
  _root = nullptr;
}

template <typename T>
bool BinarySearchTree<T>::isEmpty() {
  return !_root;
}

template <typename T>
int BinarySearchTree<T>::depth() {
  if (!_root) return 0;

  return _root->depth();
}

template <typename T>
void BinarySearchTree<T>::push(T value) {
  if (!_root)
    _root = new TreeNode<T>(value);
  else
    _root->push(value);
}

template <typename T>
bool BinarySearchTree<T>::contains(T value) {
  if (!_root) return false;

  return _root->contains(value);
}

template <typename T>
template <typename U>
BinarySearchTree<U>* BinarySearchTree<T>::map(U (*fn)(T)) {
  if (!_root) return new BinarySearchTree<U>();

  auto newTree = new BinarySearchTree<U>();
  newTree->_root = _root->map(fn);

  return newTree;
}

template <typename T>
T* BinarySearchTree<T>::find(bool (*fn)(T)) {
  if (!_root) return nullptr;

  return _root->find(fn);
}
