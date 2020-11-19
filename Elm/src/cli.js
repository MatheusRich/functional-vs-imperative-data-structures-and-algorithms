const filename = process.argv.slice(2, 3);
var Elm = require(`./${filename}`).Elm;
var main = Elm[filename].init();

process.argv.slice(3).forEach(n => main.ports.get.send(parseInt(n)));
main.ports.put.subscribe(data => console.log(data));
