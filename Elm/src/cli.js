// Link to compiled Elm code main.js
var Elm = require('./main').Elm;
var main = Elm.Main.init();

// Get data from the command line
var args = process.argv.slice(2);
var inputs = args.map(n => parseInt(n));

// Send data to the worker
inputs.forEach(i => {
  main.ports.get.send(i);
});

// Get data from the worker
main.ports.put.subscribe(function (data) {
  console.log(data);
});
