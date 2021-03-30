class LinkedListNode<T> {
  value: T;
  next: LinkedListNode<T> | null;

  constructor(value: T, next: LinkedListNode<T> | null = null) {
    this.value = value;
    this.next = next;
  }
}

export class LinkedList<T> {
  _head: LinkedListNode<T> | null;

  constructor(head: LinkedListNode<T> | null = null) {
    this._head = head;
  }

  push(value: T): LinkedList<T> {
    let newNode = new LinkedListNode(value, this._head);
    this._head = newNode;

    return this;
  }

  map(fn: (value: T) => any): LinkedList<any> {
    if (!this.head()) return new LinkedList<any>();

    let newList = new LinkedList<any>();

    let currentNode = this.head();
    while (currentNode) {
      newList.push(fn(currentNode.value));
      currentNode = currentNode.next;
    }

    return newList.reverse();
  }

  filter(fn: (a: T) => boolean): LinkedList<T> {
    if (!this.head()) return new LinkedList<T>();

    let newList = new LinkedList<T>();
    let currentNode = this.head();
    while (currentNode) {
      if (fn(currentNode.value)) newList.push(currentNode.value);
      currentNode = currentNode.next;
    }

    return newList.reverse();
  }

  reduce(fn: (a: T, b: T) => T, acc: T): T {
    if (!this.head()) return acc;

    let currentNode = this.head();
    while (currentNode) {
      acc = fn(currentNode.value, acc);
      currentNode = currentNode.next;
    }

    return acc;
  }

  reverse(): LinkedList<T> {
    let reversed = new LinkedList<T>();
    let currentNode = this._head;

    while (currentNode) {
      reversed.push(currentNode.value);
      currentNode = currentNode.next;
    }

    return reversed;
  }

  head(): LinkedListNode<T> | null {
    return this._head;
  }

  tail(): LinkedList<T> {
    if (!this._head) return new LinkedList<T>();

    return new LinkedList<T>(this._head.next);
  }

  index(index: number): LinkedListNode<T> | null {
    if (index < 0 || index >= this.length()) return null;

    let currentIndex: number = 0;
    let currentNode = this._head;

    while (currentIndex < index && currentNode && currentNode.next) {
      currentNode = currentNode.next;
      currentIndex++;
    }

    return currentNode;
  }

  last(): LinkedListNode<T> | null {
    let currentNode = this._head;
    while (currentNode && currentNode.next) {
      currentNode = currentNode.next;
    }

    return currentNode;
  }

  isEmpty(): boolean {
    return this.length() === 0;
  }

  length(): number {
    if (!this.head()) return 0;

    let length = 0;
    let currentNode = this.head();
    while (currentNode) {
      length++;
      currentNode = currentNode.next;
    }

    return length;
  }

  mergeSort(): LinkedList<T> {
    if (this.length() === 1) return this;

    const half = Math.floor(this.length() / 2);
    const left = this.take(half);
    const right = this.drop(half);

    return this.merge(left.mergeSort(), right.mergeSort());
  }

  merge(left: LinkedList<T>, right: LinkedList<T>): LinkedList<T> {
    let leftNode = left.head();
    let rightNode = right.head();
    let newList = new LinkedList<T>();

    while (leftNode && rightNode) {
      if (leftNode.value < rightNode.value) {
        newList.push(leftNode.value);
        leftNode = leftNode.next;
      } else {
        newList.push(rightNode.value);
        rightNode = rightNode.next;
      }
    }

    while (leftNode) {
      newList.push(leftNode.value);

      leftNode = leftNode.next;
    }

    while (rightNode) {
      newList.push(rightNode.value);

      rightNode = rightNode.next;
    }

    return newList.reverse();
  }

  take(n: number): LinkedList<T> {
    let newList = new LinkedList<T>();
    if (n === 0) return newList;

    let node = this.head();
    if (!node) return newList;

    for (let i = 0; i < n; i++) {
      newList.push(node.value);
      node = node.next;

      if (!node) break;
    }

    return newList.reverse();
  }

  drop(n: number): LinkedList<T> {
    let newList = new LinkedList<T>();
    if (n === this.length()) return newList;

    let node = this.head();
    if (!node) return newList;

    for (let i = 0; i < n; i++) {
      node = node.next;

      if (!node) break;
    }

    while (node) {
      newList.push(node.value);
      node = node.next;
    }

    return newList.reverse();
  }

  toString(): string {
    let currentNode = this._head;
    let str = '';
    while (currentNode) {
      str += `${currentNode.value} -> `;
      currentNode = currentNode.next;
    }
    return str + 'X';
  }
}
