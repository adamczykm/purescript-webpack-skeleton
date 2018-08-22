"use strict";

var React = require("react");

exports.forceUpdate = function(cmp) { return function() { cmp.forceUpdate(); }; };
