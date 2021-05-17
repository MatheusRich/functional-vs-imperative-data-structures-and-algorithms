import {LinkedList} from './LinkedList';

const toString = (list: LinkedList<any>): string => {
  let currentNode = list._head;
  let str = '';
  while (currentNode) {
    str += `${currentNode.value} -> `;
    currentNode = currentNode.next;
  }
  return str + 'X';
};

console.log('Creating a new linked list of integers');
let list = new LinkedList<number>();
let list2 = new LinkedList<number>();
console.log(list.isEmpty() ? 'It is empty' : 'Fail');
console.log('Pushing 3, 1, 4, 1, and 5');
list.push(3).push(1).push(4).push(1).push(5);
list2.push(3).push(1).push(4).push(1).push(5);
console.log(!list.isEmpty() ? 'It is not empty' : 'Fail');
console.log(`Length is ${list.length()}`);
console.log(`List is ${toString(list)}`);
console.log(`Head is ${list.head()}`);
console.log(`Tail is ${toString(list.tail())}`);
console.log(`Tail length is ${list.tail().length()}`);
console.log(`Last is ${list.last()}`);
console.log(`Reversed list is ${toString(list.reverse())}`);
console.log(`List as string is ${toString(list.map(i => `"${i}"`))}`);
console.log(`List without even numbers is ${toString(list2.filter(n => n % 2 == 1))}`);
console.log(`Sum of all list elements is ${list.reduce((a, b) => a + b, 0)}`);
console.log(`Sorted list is ${toString(list.mergeSort())}`);
