// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

var input = Fs.readFileSync("input/Week2/Day9Input.txt", "utf8");

var data = Belt_Array.keepMap(Belt_Array.map(input.split("\n"), Belt_Int.fromString), (function (x) {
        return x;
      }));

function arrySum(arr) {
  return Belt_Array.concatMany(Belt_Array.map(arr, (function (a) {
                    return Belt_Array.map(arr, (function (b) {
                                  if (a !== b) {
                                    return [
                                            a + b | 0,
                                            a,
                                            b
                                          ];
                                  } else {
                                    return [
                                            undefined,
                                            undefined,
                                            undefined
                                          ];
                                  }
                                }));
                  })));
}

function runPart1(_start) {
  while(true) {
    var start = _start;
    var arrPreamble = Belt_Array.slice(data, start - 25 | 0, 25);
    var target = Belt_Array.get(data, start);
    var isPass = Belt_Array.some(arrySum(arrPreamble), (function(target){
        return function (param) {
          return param[0] === target;
        }
        }(target)));
    if (!isPass) {
      return target;
    }
    _start = start + 1 | 0;
    continue ;
  };
}

function findWeaknessIndex(param) {
  var minIndex = Belt_Option.map(param[0], (function (x) {
          return data.indexOf(x);
        }));
  var maxIndex = Belt_Option.map(param[1], (function (x) {
          return data.indexOf(x);
        }));
  return [
          minIndex,
          maxIndex
        ];
}

function arrIndexNumbers(param) {
  var b = param[1];
  var a = param[0];
  if (a === undefined) {
    return [];
  }
  if (b === undefined) {
    return [];
  }
  var arr = Belt_Array.slice(data, a, (b - a | 0) + 1 | 0);
  console.log(arr);
  return arr;
}

function sum(x) {
  var res = Belt_Array.reduce(x, 0, (function (acc, x) {
          return acc + x | 0;
        }));
  console.log(res);
  return res;
}

console.log("Day 9 result part 1 :: ");

console.log(runPart1(25));

console.log("Day 9 result part 2 :: ");

var preambleSize = 25;

exports.input = input;
exports.data = data;
exports.preambleSize = preambleSize;
exports.arrySum = arrySum;
exports.runPart1 = runPart1;
exports.findWeaknessIndex = findWeaknessIndex;
exports.arrIndexNumbers = arrIndexNumbers;
exports.sum = sum;
/* input Not a pure module */
