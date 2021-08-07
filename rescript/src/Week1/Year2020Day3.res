
// let input = Lazy({Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")})
// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day3Input.txt")

type testCase = {
    hPoint : int
    , vPoint : int
}

type result = {
    point: int,
    treeCnt: int,
}

let arrCase : array<testCase> =
    [ {hPoint : 1, vPoint : 1}
    , {hPoint : 3, vPoint : 1}
    , {hPoint : 5, vPoint : 1}
    , {hPoint : 7, vPoint : 1}
    , {hPoint : 1, vPoint : 2}
    ];

let parseInput = (input) => {
    // let arrPatterns : array<string> = input |> Js.String.split("\n"); * deprecated
    let arrPatterns : array<string> = input -> Js.String2.split("\n");
    let patternLen : int = arrPatterns[0] -> Js.String.length
    (arrPatterns, patternLen)
}

let sol = (input, { hPoint, vPoint }) => {
    
    let (arrPatterns, patternLen) = input -> parseInput

    let extraction = ({ point, treeCnt }, pattern) => {
        
        // let isTree = Js.String.substrAtMost(~from=mod(point, patternLen), ~length=1, pattern) === "#"
        let addTreeCnt = pattern 
            -> Js.String2.charAt(mod(point, patternLen)) 
            -> Belt.Int.fromString 
            -> Belt.Option.getExn

        let newTreeCnt = treeCnt + addTreeCnt
        let newPoint = point + hPoint

        {
            point: newPoint,
            treeCnt: newTreeCnt,
        }
    }
    
    arrPatterns
        ->Belt.Array.keepWithIndex((_, idx) => mod(idx, vPoint) === 0)
        ->Belt.Array.map(x => x -> Js.String2.replaceByRe(%re("/[.]/g"), "0") -> Js.String2.replaceByRe(%re("/[#]/g"), "1"))
        ->Belt.Array.reduce({ point: 0, treeCnt: 0 }, extraction)
}

let sol2 = sol(input)
let result = Belt.Array.map(arrCase, sol2) 
    -> Belt.Array.reduce(1, (acc, x) => acc * x.treeCnt)

// let result = sol(input, {hPoint : 3, vPoint : 1})

result -> Js.log
