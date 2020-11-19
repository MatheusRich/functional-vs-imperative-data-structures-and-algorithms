import {BinarySearchTree as BST} from './BinarySearchTree';

console.log('Creating a new binary search tree of integers');
let tree = new BST<number>();
console.log(tree.isEmpty() ? 'It is empty' : 'Fail');
console.log('Pushing 3, -1, -4 and 5');
tree.push(3);
tree.push(-1);
tree.push(-4);
tree.push(5);
console.log(!tree.isEmpty() ? 'It is not empty' : 'Fail');
console.log(`Depth is ${tree.depth()}`);
console.log(`First negative number is ${tree.find(x => x < 0)}`);
