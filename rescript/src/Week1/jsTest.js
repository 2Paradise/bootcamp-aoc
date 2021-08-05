'use strict';

var Fs = require("fs");

// var input = Fs.readFileSync("input/Week1/Year2020Day3.sample.txt", "utf8");
var input = Fs.readFileSync("input/Week1/jsTestInput.txt", "utf8");

const arrCase = [
    {hPoint : 1, vPoint : 1}
    , {hPoint : 3, vPoint : 1}
    , {hPoint : 5, vPoint : 1}
    , {hPoint : 7, vPoint : 1}
    , {hPoint : 1, vPoint : 2}
];

const parseInput = (input) => {
    const arrPatterns = input.split('\n');
    const [pattern] = arrPatterns;
    const patternLen = pattern.length;
    return [arrPatterns, patternLen];
}

const treeCont = (acc, x) => (x === '#') ? acc + 1 : acc;

const sol = (input) => ({ hPoint, vPoint }) => {
    const [arrPatterns, patternLen] = parseInput(input);

    let point = 0;
    let accVPoint = vPoint;

    console.log(arrPatterns
        .reduce( ( acc, pattern) => {
            if(accVPoint === vPoint) {
                if(point >= patternLen) point = point - patternLen;
                acc.push(pattern.substr(point, 1));
                point = point + hPoint;
                accVPoint = 1;
            }
            else {
                accVPoint ++;
            }
            return acc;
        }, []));
    
    return arrPatterns
        .reduce( ( acc, pattern) => {
            if(accVPoint === vPoint) {
                if(point >= patternLen) point = point - patternLen;
                acc.push(pattern.substr(point, 1));
                point = point + hPoint;
                accVPoint = 1;
            }
            else {
                accVPoint ++;
            }
            return acc;
        }, [])
        .reduce(treeCont);
}

// * return: object 값에 불필요한 vCnt 를 삭제하면서 return : int 로 변경 후 해당 func 제거
// const getTreeCnt = (x) => x.treeCnt;

// sol2 :: ({ hPoint, vPoint }) -> { treeCnt, vCnt }
// map(sol2) :: Array<{ hPoint, vPoint }> -> Array<{ treeCnt, vCnt }>

const sol2 = sol(input);
const result2 = arrCase
  .map(sol2)
  .reduce((acc, x) => acc * x);

console.log('result2 :: ', result2);


// const result = arrCase.reduce((acc, cur) => {
//     const {treeCnt} = sol2(cur);
//     return acc * treeCnt;
// }, 1);

// console.log('result :: ', result);
