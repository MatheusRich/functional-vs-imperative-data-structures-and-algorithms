class BSTNode<T> {
  value: T;
  left: BSTNode<T> | null;
  right: BSTNode<T> | null;

  constructor(
    value: T,
    left: BSTNode<T> | null = null,
    right: BSTNode<T> | null = null,
  ) {
    this.value = value;
    this.left = left;
    this.right = right;
  }

  push(value: T): BSTNode<T> {
    if (value < this.value) {
      if (this.left === null) {
        this.left = new BSTNode(value);
      } else {
        this.left.push(value);
      }
    } else if (value > this.value) {
      if (this.right === null) {
        this.right = new BSTNode(value);
      } else {
        this.right.push(value);
      }
    }

    return this;
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

  map(fn: (value: T) => any): BSTNode<any> {
    if (!this.value) return new BSTNode(null);

    let newNode = new BSTNode(fn(this.value));

    if (this.left) {
      newNode.left = this.left.map(fn);
    } else {
      newNode.left = new BSTNode(null);
    }

    if (this.right) {
      newNode.right = this.right.map(fn);
    } else {
      newNode.right = new BSTNode(null);
    }

    return newNode;
  }
}

class BinarySearchTree<T> {
  root: BSTNode<T> | null;

  constructor() {
    this.root = null;
  }

  push(value: T): BinarySearchTree<T> {
    if (!this.root) {
      this.root = new BSTNode(value);
    } else {
      this.root.push(value);
    }

    return this;
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

let bst: BinarySearchTree<number> = new BinarySearchTree();
bst.push(1);
bst.push(2);
bst.push(-3);
console.log(bst.map((n) => n * 2));
