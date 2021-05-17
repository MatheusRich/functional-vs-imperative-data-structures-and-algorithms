import {measure} from 'kelonio';
import {LinkedList} from './LinkedList';
const util = require('util');
const debug = (args: any) => console.log(util.inspect(args, false, null, true));

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

async function sortSuite(
  l1: LinkedList<number>,
  l2: LinkedList<number>,
  l3: LinkedList<number>,
  l4: LinkedList<number>
) {
  let measurement = await measure(() => l1.mergeSort());
  console.log(`L1 sort Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.mergeSort());
  console.log(`L2 sort Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.mergeSort());
  console.log(`L3 sort Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l4.mergeSort());
  console.log(`L4 sort Mean: ${measurement.mean} ms`);
}

async function filterSuite(
  l1: LinkedList<number>,
  l2: LinkedList<number>,
  l3: LinkedList<number>,
  l4: LinkedList<number>,
  l5: LinkedList<number>
) {
  let measurement = await measure(() => l1.filter(isOdd), {iterations: 1});
  console.log(`L1 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.filter(isOdd), {iterations: 1});
  console.log(`L2 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.filter(isOdd), {iterations: 1});
  console.log(`L3 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l4.filter(isOdd), {iterations: 1});
  console.log(`L4 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l5.filter(isOdd), {iterations: 1});
  console.log(`L5 filter Mean: ${measurement.mean} ms`);
}

async function reduceSuite(
  l1: LinkedList<number>,
  l2: LinkedList<number>,
  l3: LinkedList<number>,
  l4: LinkedList<number>,
  l5: LinkedList<number>
) {
  let measurement = await measure(() => l1.reduce(sum, 0));
  console.log(`L1 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.reduce(sum, 0));
  console.log(`L2 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.reduce(sum, 0));
  console.log(`L3 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l4.reduce(sum, 0));
  console.log(`L4 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l5.reduce(sum, 0));
  console.log(`L4 reduce Mean: ${measurement.mean} ms`);
}

let list1 = createList(10);
let list2 = createList(100);
let list3 = createList(1000);
let list4 = createList(10000);
let list5 = createList(100000);

const bench = async () => {
  await sortSuite(list1, list2, list3, list4);
  await filterSuite(list1, list2, list3, list4, list5);
  await reduceSuite(list1, list2, list3, list4, list5);
};

bench();
