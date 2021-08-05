// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("input/Week1/jsTestInput.txt", "utf8");

function parseInput(input) {
  var arrPatterns = input.split("\n");
  var patternLen = Caml_array.get(arrPatterns, 0).length;
  return [
          arrPatterns,
          patternLen
        ];
}

function sol(input, param) {
  var vPoint = param.vPoint;
  var hPoint = param.hPoint;
  var match = parseInput(input);
  var patternLen = match[1];
  var pointRef = {
    contents: 0
  };
  var checkTree = function (treeCnt, pattern, idx) {
    if (Caml_int32.mod_(idx, vPoint) === 0) {
      var treeRef = treeCnt;
      if (pointRef.contents >= patternLen) {
        pointRef.contents = pointRef.contents - patternLen | 0;
      }
      if (pattern.substr(pointRef.contents, 1) === "#") {
        treeRef = treeRef + 1 | 0;
      }
      pointRef.contents = pointRef.contents + hPoint | 0;
      return treeRef;
    } else {
      return treeCnt;
    }
  };
  return Belt_Array.reduceWithIndex(match[0], 0, checkTree);
}

var arrCase = [
  {
    hPoint: 1,
    vPoint: 1
  },
  {
    hPoint: 3,
    vPoint: 1
  },
  {
    hPoint: 5,
    vPoint: 1
  },
  {
    hPoint: 7,
    vPoint: 1
  },
  {
    hPoint: 1,
    vPoint: 2
  }
];

function sol2(param) {
  return sol(input, param);
}

var result = Belt_Array.reduce(Belt_Array.map(arrCase, sol2), 1, (function (acc, x) {
        return Math.imul(acc, x);
      }));

console.log(result);

exports.input = input;
exports.parseInput = parseInput;
exports.sol = sol;
exports.arrCase = arrCase;
exports.sol2 = sol2;
exports.result = result;
/* input Not a pure module */
