#include <iostream>

using namespace std;

template <typename T>
class TreeNode {
public:
  T _value;
  TreeNode<T> *_left;
  TreeNode<T> *_right;

  TreeNode(T value);
  T find(bool (*fn)(T));
  bool isEmpty();
  int depth();
  void push(T value);
  bool contains(T value);
};

template <typename T>
class BinarySearchTree {
private:
  TreeNode<T> *_root;

public:
  BinarySearchTree();

  T find(bool (*fn)(T));
  bool isEmpty();
  int depth();
  void push(T value);
  bool contains(T value);
};
