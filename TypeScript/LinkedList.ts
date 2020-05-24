class LinkedList {
  list: number[];

  constructor() {
    this.list = [];
  }

  // Create

  push(value: number): number[] {
    this.list.push(value);

    return this.list;
  }

  // Transform

  // Deconstruct

  head(): number | null {
    return this.list[0];
  }

  tail(): number[] | null {
    if (this.list.length <= 1) { return null; }

    return this.list.slice(1);
  }

  index(index: number): number | null {
    if (this.list.length < index) { return null; }

    return this.list[index];
  }

  last(): number | null {
    if (this.list.length === 0) { return null; }

    return this.list[this.list.length - 1];
  }

  // Utilities

  length(): number {
    return this.list.length;
  }

  // Combine

  append(otherList : LinkedList): number[] {
    return this.list.concat(otherList.list);
  }
}

// let l = new LinkedList();
// l.push(1)
// let l2 = new LinkedList();
// l2.push(2)
// console.log(l.tail());
// console.log(l.head());
// console.log(l.index(2));
// console.log(l.append(l2));
