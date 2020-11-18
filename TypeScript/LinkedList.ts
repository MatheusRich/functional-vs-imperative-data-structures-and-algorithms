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
  _length: number;

  constructor(head: LinkedListNode<T> | null = null, length: number = 0) {
    this._head = head;
    this._length = length;
  }

  // Create

  push(value: T): LinkedList<T> {
    let newNode = new LinkedListNode(value, this._head);
    this._head = newNode;
    this._length++;

    return this;
  }

  // Transform

  // TODO: REVIEW THIS
  map(func: (value: T) => any): LinkedList<T> {
    if (!this.head()) return new LinkedList<T>();

    let newList = new LinkedList<T>();

    let currentNode = this.head();
    while (currentNode) {
      newList.push(func(currentNode.value));
      currentNode = currentNode.next;
    }

    return newList.reverse();
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

  // Deconstruct

  head(): LinkedListNode<T> | null {
    return this._head;
  }

  tail(): LinkedList<T> {
    if (!this._head) return new LinkedList<T>();

    return new LinkedList<T>(this._head.next, this._length - 1);
  }

  index(index: number): LinkedListNode<T> | null {
    if (this._length < index || index < 0) return null;

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

  // Utilities

  isEmpty(): boolean {
    return this._length === 0;
  }

  length(): number {
    return this._length;
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

  // Combine

  append(otherList: LinkedList<T>): LinkedList<T> {
    let currentNode = this.head();
    while (currentNode && currentNode.next) {
      currentNode = currentNode.next;
    }

    if (currentNode) {
      currentNode.next = otherList.head();
    } else {
      currentNode = otherList.head();
    }

    this._length += otherList.length();

    return this;
  }
}
