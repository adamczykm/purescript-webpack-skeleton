"use strict";

var React = require("react");
var ReactDOM = require("react-dom");
var Main = require('./output/Main/index');
console.log('hello worldx');
ReactDOM.render(
    React.createElement(Main.main(), {}),
    document.getElementById("app")
);
