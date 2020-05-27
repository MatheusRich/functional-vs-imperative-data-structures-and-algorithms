class LinkedListNode<T> {
  value: T;
  next: LinkedListNode<T> | null;

  constructor(value: T, next: LinkedListNode<T> | null = null) {
    this.value = value;
    this.next = next;
  }
}

class LinkedList<T> {
  head: LinkedListNode<T> | null;
  length: number;

  constructor() {
    this.head = null;
    this.length = 0;
  }

  // Create

  push(value: T): LinkedList<T> {
    let newNode = new LinkedListNode(value, this.head);
    this.head = newNode;
    this.length++;

    return this;
  }

  // Transform

  // map(func: Function): LinkedList<T> {
  //   if (!this.head) return new LinkedList();

  //   while (head) {}

  //   return;
  // }

  // reverse()

  // Deconstruct

  // head(): LinkedListNode<T> | null {
  //   return this.head;
  // }

  tail(): LinkedListNode<T> | null {
    if (!this.head) {
      return null;
    }

    return this.head.next;
  }

  index(index: number): LinkedListNode<T> | null {
    if (this.length < index || index < 0) return null;

    let currentIndex: number = 0;
    let currentNode = this.head;

    while (currentIndex < index && currentNode && currentNode.next) {
      currentNode = currentNode.next;
      currentIndex++;
    }

    return currentNode;
  }

  last(): LinkedListNode<T> | null {
    let currentNode = this.head;
    while (currentNode && currentNode.next) {
      currentNode = currentNode.next;
    }

    return currentNode;
  }

  // // Utilities

  // get length(): number {
  //   return this.length;
  // }

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
