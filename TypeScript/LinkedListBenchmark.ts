import {measure} from 'kelonio';
import {LinkedList} from './LinkedList';

function createList(size: number): LinkedList<number> {
  let list = new LinkedList<number>();

  for (let i = 0; i < size; i++) {
    list.push(i + 1);
  }

  return list;
}

async function sortSuite(l1: LinkedList<number>, l2: LinkedList<number>, l3: LinkedList<number>) {
  let measurement = await measure(() => l1.mergeSort());
  console.log(`L1 sort Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.mergeSort());
  console.log(`L2 sort Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.mergeSort());
  console.log(`L3 sort Mean: ${measurement.mean} ms\n`);
}

async function filterSuite(l1: LinkedList<number>, l2: LinkedList<number>, l3: LinkedList<number>) {
  let measurement = await measure(() => l1.filter(n => n / 2 == 1));
  console.log(`L1 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.filter(n => n / 2 == 1));
  console.log(`L2 filter Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.filter(n => n / 2 == 1));
  console.log(`L3 filter Mean: ${measurement.mean} ms\n`);
}

async function reduceSuite(l1: LinkedList<number>, l2: LinkedList<number>, l3: LinkedList<number>) {
  let measurement = await measure(() => l1.reduce((a, b) => a + b, 0));
  console.log(`L1 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l2.reduce((a, b) => a + b, 0));
  console.log(`L2 reduce Mean: ${measurement.mean} ms`);

  measurement = await measure(() => l3.reduce((a, b) => a + b, 0));
  console.log(`L3 reduce Mean: ${measurement.mean} ms`);
}

let list1 = createList(1000);
let list2 = createList(10000);
let list3 = createList(100000);

const bench = async () => {
  await sortSuite(list1, list2, list3);
  await filterSuite(list1, list2, list3);
  await reduceSuite(list1, list2, list3);
};
bench()
