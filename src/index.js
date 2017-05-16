require('./index.html');
require('./main.css');

const Elm = require('./Main.elm');
const mountNode = document.getElementById('main');

const app = Elm.Main.embed(mountNode);

// load ace editor
require('brace');
const aceCodeBox = require('./aceCodeBox');
aceCodeBox.subscribe(app);