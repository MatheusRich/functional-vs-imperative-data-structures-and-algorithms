class TreeNode<T> {
  value: T;
  left: TreeNode<T> | null;
  right: TreeNode<T> | null;

  constructor(value: T) {
    this.value = value;
    this.left = null;
    this.right = null;
  }

  depth(): number {
    const leftDepth = this.left?.depth() || 0;
    const rightDepth = this.right?.depth() || 0;

    return 1 + Math.max(leftDepth, rightDepth);
  }

  push(value: T): TreeNode<T> {
    if (value < this.value) {
      if (this.left === null) this.left = new TreeNode(value);
      else this.left.push(value);
    } else if (value > this.value) {
      if (this.right === null) this.right = new TreeNode(value);
      else this.right.push(value);
    }

    return this;
  }

  find(fn: (value: T) => boolean): T | null {
    if (fn(this.value)) return this.value;

    let leftFind = this.left ? this.left.find(fn) : null;
    if (leftFind) return leftFind;

    let rightFind = this.right ? this.right.find(fn) : null;
    if (rightFind) return rightFind;

    return null;
  }

  contains(value: T): boolean {
    if (value < this.value) {
      if (!this.left) return false;
      return this.left.contains(value);
    } else if (value > this.value) {
      if (!this.right) return false;
      return this.right.contains(value);
    }

    return true;
  }

  map(fn: (value: T) => any): TreeNode<any> {
    let newNode = new TreeNode(fn(this.value));

    if (this.left) newNode.left = this.left.map(fn);
    if (this.right) newNode.right = this.right.map(fn);

    return newNode;
  }
}

export class BinarySearchTree<T> {
  root: TreeNode<T> | null;

  constructor() {
    this.root = null;
  }

  isEmpty(): boolean {
    return this.root === null;
  }

  depth(): number {
    if (!this.root) return 0;

    return this.root.depth();
  }

  push(value: T): BinarySearchTree<T> {
    if (!this.root) this.root = new TreeNode(value);
    else this.root.push(value);

    return this;
  }

  find(fn: (value: T) => boolean): T | null {
    if (!this.root) return null;

    return this.root.find(fn);
  }

  contains(value: T): boolean {
    if (!this.root) return false;

    return this.root.contains(value);
  }

  map(fn: (value: T) => any): BinarySearchTree<any> {
    if (!this.root) return new BinarySearchTree();

    let newTree = new BinarySearchTree();
    newTree.root = this.root.map(fn);

    return newTree;
  }
}
