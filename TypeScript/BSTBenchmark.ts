import {BinarySearchTree} from './BinarySearchTree';
const { add, cycle, suite, complete } = require('benny');
const util = require('util');
const debug = (args: any) => console.log(util.inspect(args, false, null, true));

function listFromRange(begin: number, end: number): number[] {
  let l = [];

  for (let i = begin - 1; i < end; i++) l.push(i + 1);

  return l;
}

function treeFromList(list: number[], tree: BinarySearchTree<number>) {
  if (list.length === 0) return;
  if (list.length === 1) {
    tree.push(list[0]);
    return;
  }

  const middleIdx = Math.floor(list.length / 2);
  tree.push(list[middleIdx]);

  treeFromList(list.slice(0, middleIdx), tree);
  treeFromList(list.slice(middleIdx + 1), tree);
}

function treeFromRange(from: number, to: number) {
  let tree = new BinarySearchTree<number>();
  treeFromList(listFromRange(from, to), tree);

  return tree;
}

function doubleN(n: number) {
  return n * 2;
}

const containsSuite = (...sizes: number[]) => {
  return sizes.map((size) => {
    return add(`${size} elements`, () => {
      const input = treeFromRange(1, size)

      return () => input.contains(size)
    });
  });
}

const mapSuite = (...sizes: number[]) => {
  return sizes.map((size) => {
    return add(`${size} elements`, () => {
      const input = treeFromRange(1, size)

      return () => input.map(doubleN)
    });
  });
}

const batches = [10, 100, 500, 1000, 2500, 5000, 7500, 10000];

suite(
  'Contains',
  ...containsSuite(...batches),
  cycle(),
  complete(),
)

suite(
  'Map',
  ...mapSuite(...batches),
  cycle(),
  complete(),
)
