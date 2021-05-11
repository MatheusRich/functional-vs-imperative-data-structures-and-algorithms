import {measure} from 'kelonio';
import {BinarySearchTree} from './BinarySearchTree';
const util = require('util');
const debug = (args: any) => console.log(util.inspect(args, false, null, true));

function listFromRange(begin: number, end: number): number[] {
  let l = [];

  for (let i = begin - 1; i < end; i++) l.push(i + 1);

  return l;
}

function treeFromList(list: number[], tree: BinarySearchTree<number>) {
  if (list.length === 0) return;

  const middleIdx = Math.floor(list.length / 2);
  tree.push(list[middleIdx]);

  if (list.length === 1) return;

  treeFromList(list.slice(0, middleIdx), tree);
  treeFromList(list.slice(middleIdx), tree);
}

function treeFromRange(from: number, to: number) {
  let tree = new BinarySearchTree<number>();
  treeFromList(listFromRange(from, to), tree);

  return tree;
}

function doubleN(n: number) {
  return n * 2;
}

async function containsSuite(
  t1: BinarySearchTree<number>,
  t2: BinarySearchTree<number>,
  t3: BinarySearchTree<number>,
  t4: BinarySearchTree<number>
) {
  let measurement = await measure(() => t1.contains(6));
  console.log(`T1 contains Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t2.contains(51));
  console.log(`T2 contains Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t3.contains(501));
  console.log(`T3 contains Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t4.contains(5001));
  console.log(`T4 contains Mean: ${measurement.mean} ms`);
}

async function mapSuite(
  t1: BinarySearchTree<number>,
  t2: BinarySearchTree<number>,
  t3: BinarySearchTree<number>,
  t4: BinarySearchTree<number>
) {
  let measurement = await measure(() => t1.map(doubleN));
  console.log(`T1 map Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t2.map(doubleN));
  console.log(`T2 map Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t3.map(doubleN));
  console.log(`T3 map Mean: ${measurement.mean} ms`);

  measurement = await measure(() => t4.map(doubleN));
  console.log(`T4 map Mean: ${measurement.mean} ms`);
}

let tree1 = treeFromRange(1, 10);
let tree2 = treeFromRange(1, 100);
let tree3 = treeFromRange(1, 1000);
let tree4 = treeFromRange(1, 10000);

// debug(tree1);

const bench = async () => {
  await containsSuite(tree1, tree2, tree3, tree4);
  await mapSuite(tree1, tree2, tree3, tree4);
};

bench();
