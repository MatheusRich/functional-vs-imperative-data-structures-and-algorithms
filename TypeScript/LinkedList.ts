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

  // TODO: REVIEW THIS
  map(fn: (value: T) => any): LinkedList<T> {
    if (!this.head()) return new LinkedList<T>();

    let newList = new LinkedList<T>();

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
