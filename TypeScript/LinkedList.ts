class LinkedListNode<T> {
  value: T;
  next: LinkedListNode<T> | null;

  constructor(value: T, next: LinkedListNode<T> | null = null) {
    this.value = value;
    this.next = next;
  }
}

class LinkedList<T> {
  _head: LinkedListNode<T> | null;
  _length: number;

  constructor() {
    this._head = null;
    this._length = 0;
  }

  // Create

  push(value: T): LinkedList<T> {
    let newNode = new LinkedListNode(value, this._head);
    this._head = newNode;
    this._length++;

    return this;
  }

  // Transform

  // map(func: Function): LinkedList<T> {
  //   if (!this._head) return new LinkedList();

  //   while (head) {}

  //   return;
  // }

  // reverse()

  // Deconstruct

  head(): LinkedListNode<T> | null {
    return this._head;
  }

  tail(): LinkedListNode<T> | null {
    if (!this._head) {
      return null;
    }

    return this._head.next;
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

  // // Utilities

  get length(): number {
    return this._length;
  }

  // // Combine

  // append(otherList: LinkedList<T>): T[] {
  //   return this.list.concat(otherList.list);
  // }
}

let l = new LinkedList();
l.push(1);
l.push(2);
l.push(3);
// l.push('3');

// let l2 = new LinkedList();
// l2.push('2')
// console.log(l.index(10));
// console.log(l);
// console.log(l.last());
// console.log(l.index(2));
// console.log(l.append(l2));
