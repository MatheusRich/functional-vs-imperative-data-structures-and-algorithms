class BSTNode<T> {
  value: T;
  left: BSTNode<T> | null;
  right: BSTNode<T> | null;

  constructor(value: T, left: BSTNode<T> | null = null, right: BSTNode<T> | null = null) {
    this.value = value;
    this.left = left;
    this.right = right;
  }

  depth(): number {
    const leftDepth = this.left?.depth() || 0;
    const rightDepth = this.right?.depth() || 0;

    return 1 + Math.max(leftDepth, rightDepth);
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

  find(fn: (value: T) => boolean): T | null {
    if (fn(this.value)) return this.value;

    let leftFind = null;
    if (this.left) leftFind = this.left.find(fn);
    if (leftFind) return leftFind;

    let rightFind = null;
    if (this.right) rightFind = this.right.find(fn);
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

  toString(): string {
    return this.printSelf('', this, 0);
  }

  toString2(): string {
    let buffer = '';
    buffer = this.printSelf2(buffer, '', '');
    return buffer;
  }

  printSelf2(buffer: string, prefix: string, childrenPrefix: string): string {
    buffer += prefix;
    buffer += this.value;
    buffer += '\n';
    if (this.right) {
      buffer = this.right.printSelf2(buffer, childrenPrefix + '├── ', childrenPrefix + '│   ');
    }
    if (this.left) {
      buffer = this.left.printSelf2(buffer, childrenPrefix + '└── ', childrenPrefix + '    ');
    }

    return buffer;
  }

  printSelf(buffer: string, root: BSTNode<T> | null, level: number): string {
    if (!root) return buffer;

    buffer = this.printSelf(buffer, root.right, level + 1) + '\n';
    if (level != 0) {
      for (let i = 0; i < level - 1; i++) buffer += '│    ';
      buffer += `├──(${root.value})`;
    } else buffer += `(${root.value})`;
    buffer = this.printSelf(buffer, root.left, level + 1);
    return buffer;
  }
}

class BinarySearchTree<T> {
  root: BSTNode<T> | null;

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
    if (!this.root) {
      this.root = new BSTNode(value);
    } else {
      this.root.push(value);
    }

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

  toString(): string {
    if (!this.root) return '';

    return this.root.toString();
  }

  toString2(): string {
    if (!this.root) return '';

    return this.root.toString2();
  }
}

let bst: BinarySearchTree<number> = new BinarySearchTree();
bst.push(1);
bst.push(2);
bst.push(1.5);
bst.push(-3);
bst.push(4);
bst.push(3);
bst.push(-5);
bst.push(-4);
bst.push(-12);
bst.push(12);
// bst.toString();
console.log(`Tree is \n${bst.toString2()}`);
console.log('-------');
console.log(`Tree is \n${bst.toString()}`);
console.log(`Depth is ${bst.depth()}`);

// console.log({depth: bst.depth()});
// console.log({map: bst.map(n => n * 2)});
// console.log({find: bst.find(n => n < 0)});
