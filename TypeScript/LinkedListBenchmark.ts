import {LinkedList} from './LinkedList';
const b = require('benny')

function createList(size: number): LinkedList<number> {
  let list = new LinkedList<number>();

  for (let i = 0; i < size; i++) {
    list.push(i + 1);
  }

  return list;
}

function isOdd(n: number): boolean {
  return n % 2 === 1;
}

function sum(a: number, b: number): number {
  return a + b;
}

let l1 = createList(10);
let l2 = createList(100);
let l3 = createList(1000);
let l4 = createList(10000);

b.suite(
  'sort',

  b.add('10', () => {
    return () => l1.mergeSort()
  }),
  b.add('100', () => {
    return () => l2.mergeSort()
  }),
  b.add('1000', () => {
    return () => l3.mergeSort()
  }),
  b.add('10000', () => {
    return () => l4.mergeSort()
  }),

  b.cycle(),
  b.complete(),
)

b.suite(
  'reduce',

  b.add('10', () => {
    return () => l1.reduce(sum, 0)
  }),
  b.add('100', () => {
    return () => l2.reduce(sum, 0)
  }),
  b.add('1000', () => {
    return () => l3.reduce(sum, 0)
  }),
  b.add('10000', () => {
    return () => l4.reduce(sum, 0)
  }),

  b.cycle(),
  b.complete(),
)

b.suite(
  'filter',

  b.add('10', () => {
    return () => l1.filter(isOdd)
  }),
  b.add('100', () => {
    return () => l2.filter(isOdd)
  }),
  b.add('1000', () => {
    return () => l3.filter(isOdd)
  }),
  b.add('10000', () => {
    return () => l4.filter(isOdd)
  }),

  b.cycle(),
  b.complete(),
)
