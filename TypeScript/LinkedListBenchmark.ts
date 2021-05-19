import {LinkedList} from './LinkedList';
// const b = require('benny')
const { add, cycle, suite, complete } = require('benny');

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

const sortSuite = (...sizes: number[]) => {
  return sizes.map((size) => {
    return add(`${size} elements`, () => {
      const input = createList(size)

      return () => input.mergeSort()
    });
  });
}

const filterSuite = (...sizes: number[]) => {
  return sizes.map((size) => {
    return add(`${size} elements`, () => {
      const input = createList(size)

      return () => input.filter(isOdd)
    });
  });
}

const reduceSuite = (...sizes: number[]) => {
  return sizes.map((size) => {
    return add(`${size} elements`, () => {
      const input = createList(size)

      return () => input.reduce(sum, 0)
    });
  });
}

const batches = [10, 100, 500, 1000, 2500, 5000, 7500, 10000];

suite(
  'Sort',
  ...sortSuite(...batches),
  cycle(),
  complete(),
)

suite(
  'Filter',
  ...filterSuite(...batches),
  cycle(),
  complete(),
)

suite(
  'Reduce',
  ...reduceSuite(...batches),
  cycle(),
  complete(),
)
