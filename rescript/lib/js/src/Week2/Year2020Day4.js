// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

var input = Fs.readFileSync("input/Week2/Day4Input.txt", "utf8");

var initPassport = {
  byr: /* None */0,
  iyr: /* None */0,
  eyr: /* None */0,
  hgt: /* None */0,
  hcl: /* None */0,
  ecl: /* None */0,
  pid: /* None */0
};

function parseRecord(x) {
  return Belt_Array.reduce(x, initPassport, (function (acc, x) {
                var match = x.split(":");
                if (match.length !== 2) {
                  return acc;
                }
                var match$1 = match[0];
                switch (match$1) {
                  case "byr" :
                      var value = match[1];
                      return {
                              byr: {
                                TAG: /* Byr */0,
                                _0: value
                              },
                              iyr: acc.iyr,
                              eyr: acc.eyr,
                              hgt: acc.hgt,
                              hcl: acc.hcl,
                              ecl: acc.ecl,
                              pid: acc.pid
                            };
                  case "ecl" :
                      var value$1 = match[1];
                      var eclType;
                      switch (value$1) {
                        case "amb" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "amb"
                            };
                            break;
                        case "blu" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "blu"
                            };
                            break;
                        case "brn" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "brn"
                            };
                            break;
                        case "grn" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "grn"
                            };
                            break;
                        case "gry" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "gry"
                            };
                            break;
                        case "hzl" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "hzl"
                            };
                            break;
                        case "oth" :
                            eclType = {
                              TAG: /* Ecl */6,
                              _0: "oth"
                            };
                            break;
                        default:
                          eclType = /* None */0;
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
                      var value$2 = match[1];
                      return {
                              byr: acc.byr,
                              iyr: acc.iyr,
                              eyr: {
                                TAG: /* Eyr */2,
                                _0: value$2
                              },
                              hgt: acc.hgt,
                              hcl: acc.hcl,
                              ecl: acc.ecl,
                              pid: acc.pid
                            };
                  case "hcl" :
                      var value$3 = match[1];
                      return {
                              byr: acc.byr,
                              iyr: acc.iyr,
                              eyr: acc.eyr,
                              hgt: acc.hgt,
                              hcl: {
                                TAG: /* Hcl */5,
                                _0: value$3
                              },
                              ecl: acc.ecl,
                              pid: acc.pid
                            };
                  case "hgt" :
                      var value$4 = match[1];
                      var match$2 = value$4.replace(/\d/g, "");
                      var hgtType;
                      switch (match$2) {
                        case "cm" :
                            hgtType = {
                              TAG: /* HgtCm */3,
                              _0: value$4
                            };
                            break;
                        case "in" :
                            hgtType = {
                              TAG: /* HgtIn */4,
                              _0: value$4
                            };
                            break;
                        default:
                          hgtType = /* None */0;
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
                      var value$5 = match[1];
                      return {
                              byr: acc.byr,
                              iyr: {
                                TAG: /* Iyr */1,
                                _0: value$5
                              },
                              eyr: acc.eyr,
                              hgt: acc.hgt,
                              hcl: acc.hcl,
                              ecl: acc.ecl,
                              pid: acc.pid
                            };
                  case "pid" :
                      var value$6 = match[1];
                      return {
                              byr: acc.byr,
                              iyr: acc.iyr,
                              eyr: acc.eyr,
                              hgt: acc.hgt,
                              hcl: acc.hcl,
                              ecl: acc.ecl,
                              pid: {
                                TAG: /* Pid */7,
                                _0: value$6
                              }
                            };
                  default:
                    return acc;
                }
              }));
}

function parsePassport(acc, x) {
  return Belt_Array.concat(acc, [parseRecord(x)]);
}

function isNotEmpty(x) {
  if (typeof x === "number") {
    return false;
  } else {
    return true;
  }
}

function countPassport(x) {
  return Belt_Array.reduce(x, 0, (function (acc, x) {
                var match = isNotEmpty(x.byr);
                var match$1 = isNotEmpty(x.iyr);
                var match$2 = isNotEmpty(x.eyr);
                var match$3 = isNotEmpty(x.hgt);
                var match$4 = isNotEmpty(x.hcl);
                var match$5 = isNotEmpty(x.ecl);
                var match$6 = isNotEmpty(x.pid);
                if (match && match$1 && match$2 && match$3 && match$4 && match$5 && match$6) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }));
}

function splitPassPort(x) {
  return x.replace(/\n/g, " ").split(" ");
}

var arrInput = input.split("\n\n");

console.log("Day 4 Part 1 ::");

var arrPassport = Belt_Array.reduce(Belt_Array.map(arrInput, splitPassPort), [], parsePassport);

console.log(countPassport(arrPassport));

console.log("Day 4 Part 2 ::");

function checkRangeType(x, param) {
  var value = Belt_Option.getExn(Belt_Int.fromString(x));
  if (param[0] <= value) {
    return value <= param[1];
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

function passportValid(x) {
  if (typeof x === "number") {
    return false;
  }
  switch (x.TAG | 0) {
    case /* Byr */0 :
        var v = x._0;
        if (checkRangeType(v, [
                1920,
                2002
              ])) {
          return v.length === 4;
        } else {
          return false;
        }
    case /* Iyr */1 :
        var v$1 = x._0;
        if (checkRangeType(v$1, [
                2010,
                2020
              ])) {
          return v$1.length === 4;
        } else {
          return false;
        }
    case /* Eyr */2 :
        var v$2 = x._0;
        if (checkRangeType(v$2, [
                2020,
                2030
              ])) {
          return v$2.length === 4;
        } else {
          return false;
        }
    case /* HgtCm */3 :
        return checkRangeType(x._0, [
                    150,
                    193
                  ]);
    case /* HgtIn */4 :
        return checkRangeType(x._0, [
                    59,
                    76
                  ]);
    case /* Hcl */5 :
        return checkHex(x._0);
    case /* Ecl */6 :
        return checkEyeColor(x._0);
    case /* Pid */7 :
        return x._0.length === 9;
    
  }
}

function validatePassports(x) {
  return Belt_Array.reduce(x, 0, (function (acc, x) {
                var match = passportValid(x.byr);
                var match$1 = passportValid(x.iyr);
                var match$2 = passportValid(x.eyr);
                var match$3 = passportValid(x.hgt);
                var match$4 = passportValid(x.hcl);
                var match$5 = passportValid(x.ecl);
                var match$6 = passportValid(x.pid);
                if (match && match$1 && match$2 && match$3 && match$4 && match$5 && match$6) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }));
}

console.log(validatePassports(arrPassport));

exports.input = input;
exports.initPassport = initPassport;
exports.parseRecord = parseRecord;
exports.parsePassport = parsePassport;
exports.isNotEmpty = isNotEmpty;
exports.countPassport = countPassport;
exports.splitPassPort = splitPassPort;
exports.arrInput = arrInput;
exports.arrPassport = arrPassport;
exports.checkRangeType = checkRangeType;
exports.checkLength = checkLength;
exports.checkHex = checkHex;
exports.checkEyeColor = checkEyeColor;
exports.passportValid = passportValid;
exports.validatePassports = validatePassports;
/* input Not a pure module */
