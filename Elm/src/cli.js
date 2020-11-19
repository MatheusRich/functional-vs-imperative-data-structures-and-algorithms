var Elm = require('./main').Elm;
var main = Elm.Main.init();

process.argv.slice(2).forEach(n => main.ports.get.send(parseInt(n)));
main.ports.put.subscribe(data => console.log(data));
