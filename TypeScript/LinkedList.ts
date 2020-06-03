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

  length(): number {
    return this._length;
  }

  print() {
    let currentNode = this._head;
    let str = '';
    while (currentNode) {
      str += `${currentNode.value} -> `;
      currentNode = currentNode.next;
    }
    console.log(str + 'X');
  }

  // // Combine

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

let l = new LinkedList();
l.push(1);
l.push(2);
l.push(3);

let l2 = new LinkedList();
l2.push(4);
l2.push(5);
l2.push(6);

console.log('l1');
l.print();
console.log('\nl2');
l2.print();
console.log('\nl2 + l');
l2.append(l).print();
console.log(l2.length());
// console.log(l.last());
// console.log(l.index(2));
// console.log(l.append(l2));
