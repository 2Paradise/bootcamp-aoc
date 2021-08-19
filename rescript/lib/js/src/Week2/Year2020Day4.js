// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

var input = Fs.readFileSync("input/Week2/Day4Input.txt", "utf8");

var initPassport = {
  byr: 0,
  iyr: 0,
  eyr: 0,
  hgt: {
    TAG: /* Cm */0,
    _0: 0
  },
  hcl: "",
  ecl: "none",
  pid: ""
};

function parseStringToInt(x) {
  var i = Belt_Int.fromString(x);
  if (i !== undefined) {
    return i;
  } else {
    return 0;
  }
}

function parseRecord(x) {
  return Belt_Array.reduce(x, initPassport, (function (acc, x) {
                return Belt_Option.map(x, (function (x) {
                              if (acc === undefined) {
                                return initPassport;
                              }
                              var match = x.split(":");
                              if (match.length !== 2) {
                                return acc;
                              }
                              var match$1 = match[0];
                              switch (match$1) {
                                case "byr" :
                                    var v = match[1];
                                    return {
                                            byr: parseStringToInt(v),
                                            iyr: acc.iyr,
                                            eyr: acc.eyr,
                                            hgt: acc.hgt,
                                            hcl: acc.hcl,
                                            ecl: acc.ecl,
                                            pid: acc.pid
                                          };
                                case "ecl" :
                                    var v$1 = match[1];
                                    var eclType;
                                    switch (v$1) {
                                      case "amb" :
                                          eclType = "amb";
                                          break;
                                      case "blu" :
                                          eclType = "blu";
                                          break;
                                      case "brn" :
                                          eclType = "brn";
                                          break;
                                      case "grn" :
                                          eclType = "grn";
                                          break;
                                      case "gry" :
                                          eclType = "gry";
                                          break;
                                      case "hzl" :
                                          eclType = "hzl";
                                          break;
                                      case "oth" :
                                          eclType = "oth";
                                          break;
                                      default:
                                        eclType = "none";
                                    }
                                    return {
                                            byr: acc.byr,
                                            iyr: acc.iyr,
                                            eyr: acc.eyr,
                                            hgt: acc.hgt,
                                            hcl: acc.hcl,
                                            ecl: eclType,
                                            pid: acc.pid
                                          };
                                case "eyr" :
                                    var v$2 = match[1];
                                    return {
                                            byr: acc.byr,
                                            iyr: acc.iyr,
                                            eyr: parseStringToInt(v$2),
                                            hgt: acc.hgt,
                                            hcl: acc.hcl,
                                            ecl: acc.ecl,
                                            pid: acc.pid
                                          };
                                case "hcl" :
                                    var v$3 = match[1];
                                    return {
                                            byr: acc.byr,
                                            iyr: acc.iyr,
                                            eyr: acc.eyr,
                                            hgt: acc.hgt,
                                            hcl: v$3,
                                            ecl: acc.ecl,
                                            pid: acc.pid
                                          };
                                case "hgt" :
                                    var v$4 = match[1];
                                    var match$2 = v$4.replace(/\d/g, "");
                                    var hgtType;
                                    switch (match$2) {
                                      case "cm" :
                                          hgtType = {
                                            TAG: /* Cm */0,
                                            _0: parseStringToInt(v$4)
                                          };
                                          break;
                                      case "in" :
                                          hgtType = {
                                            TAG: /* In */1,
                                            _0: parseStringToInt(v$4)
                                          };
                                          break;
                                      default:
                                        hgtType = {
                                          TAG: /* Cm */0,
                                          _0: 0
                                        };
                                    }
                                    return {
                                            byr: acc.byr,
                                            iyr: acc.iyr,
                                            eyr: acc.eyr,
                                            hgt: hgtType,
                                            hcl: acc.hcl,
                                            ecl: acc.ecl,
                                            pid: acc.pid
                                          };
                                case "iyr" :
                                    var v$5 = match[1];
                                    return {
                                            byr: acc.byr,
                                            iyr: parseStringToInt(v$5),
                                            eyr: acc.eyr,
                                            hgt: acc.hgt,
                                            hcl: acc.hcl,
                                            ecl: acc.ecl,
                                            pid: acc.pid
                                          };
                                case "pid" :
                                    var v$6 = match[1];
                                    return {
                                            byr: acc.byr,
                                            iyr: acc.iyr,
                                            eyr: acc.eyr,
                                            hgt: acc.hgt,
                                            hcl: acc.hcl,
                                            ecl: acc.ecl,
                                            pid: v$6
                                          };
                                default:
                                  return acc;
                              }
                            }));
              }));
}

var parsePassport = parseRecord;

function sumInt(arr) {
  return Belt_Array.reduce(arr, 0, (function (acc, x) {
                return acc + x | 0;
              }));
}

function passPortCheck(x) {
  return Belt_Option.map(x, (function (param) {
                if (param.byr !== 0 && param.iyr !== 0 && param.eyr !== 0 && param.hgt !== ({
                      TAG: /* Cm */0,
                      _0: 0
                    }) && param.hcl !== "" && param.ecl !== "none" && param.pid !== "") {
                  return 1;
                } else {
                  return 0;
                }
              }));
}

var arrInput = input.split("\n\n");

function splitPassPort(x) {
  return x.replace(/\n/g, " ").split(" ");
}

var arrPassport = Belt_Array.map(Belt_Array.map(arrInput, splitPassPort), parsePassport);

console.log("Day 4 Part 1 ::");

console.log(sumInt(Belt_Array.keepMap(Belt_Array.map(arrPassport, passPortCheck), (function (x) {
                return x;
              }))));

function checkRangeType(x, param) {
  if (Caml_obj.caml_lessequal(param[0], x)) {
    return Caml_obj.caml_lessequal(x, param[1]);
  } else {
    return false;
  }
}

function checkLength(x, len) {
  return x.length === len;
}

function checkHex(x) {
  return /^[\#][0-9a-f]{6}$/.test(x);
}

function checkEyeColor(x) {
  if (x === "oth" || x === "hzl" || x === "gry" || x === "grn" || x === "brn" || x === "blu") {
    return true;
  } else {
    return x === "amb";
  }
}

function checkHgt(x) {
  if (x.TAG === /* Cm */0) {
    return checkRangeType(x._0, [
                150,
                193
              ]);
  } else {
    return checkRangeType(x._0, [
                59,
                76
              ]);
  }
}

function passportDiv(x) {
  return Belt_Option.map(x, (function (param) {
                var match = checkRangeType(param.byr, [
                      1920,
                      2002
                    ]);
                var match$1 = checkRangeType(param.iyr, [
                      2010,
                      2020
                    ]);
                var match$2 = checkRangeType(param.eyr, [
                      2020,
                      2030
                    ]);
                var match$3 = checkHgt(param.hgt);
                var match$4 = checkHex(param.hcl);
                var match$5 = checkEyeColor(param.ecl);
                var match$6 = param.pid.length === 9;
                if (match && match$1 && match$2 && match$3 && match$4 && match$5 && match$6) {
                  return 1;
                } else {
                  return 0;
                }
              }));
}

console.log("Day 4 Part 2 ::");

console.log(sumInt(Belt_Array.keepMap(Belt_Array.map(arrPassport, passportDiv), (function (x) {
                return x;
              }))));

exports.input = input;
exports.initPassport = initPassport;
exports.parseStringToInt = parseStringToInt;
exports.parseRecord = parseRecord;
exports.parsePassport = parsePassport;
exports.sumInt = sumInt;
exports.passPortCheck = passPortCheck;
exports.arrInput = arrInput;
exports.splitPassPort = splitPassPort;
exports.arrPassport = arrPassport;
exports.checkRangeType = checkRangeType;
exports.checkLength = checkLength;
exports.checkHex = checkHex;
exports.checkEyeColor = checkEyeColor;
exports.checkHgt = checkHgt;
exports.passportDiv = passportDiv;
/* input Not a pure module */
