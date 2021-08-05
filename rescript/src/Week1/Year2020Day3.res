
// let input = Lazy({Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")})
// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/jsTestInput.txt")

type testCase = {
    hPoint : int
    , vPoint : int
}

let parseInput = (input) => {
    let arrPatterns : array<string> = input |> Js.String.split("\n");
    let patternLen : int = arrPatterns[0] -> Js.String.length
    (arrPatterns, patternLen)
}

let sol = (input, { hPoint, vPoint }) => {

    let (arrPatterns, patternLen) = input -> parseInput
    let pointRef = ref(0);

    let extraction = (treeCnt, pattern) => {
        let treeRef = ref(treeCnt);
        
        if pointRef.contents >= patternLen {
            pointRef:= pointRef.contents - patternLen
        }

        if Js.String.substrAtMost(~from=pointRef.contents, ~length=1, pattern) === "#" {
            treeRef := treeRef.contents + 1
        }

        pointRef:= pointRef.contents + hPoint
        treeRef.contents
    }
    
    let checkTree = (treeCnt, pattern, idx) => {
        mod(idx, vPoint) === 0 ? extraction(treeCnt, pattern) : treeCnt
    }

    Belt.Array.reduceWithIndex(arrPatterns, 0, checkTree)
}

let arrCase : array<testCase> = [
    {hPoint : 1, vPoint : 1}
    , {hPoint : 3, vPoint : 1}
    , {hPoint : 5, vPoint : 1}
    , {hPoint : 7, vPoint : 1}
    , {hPoint : 1, vPoint : 2}
];

let sol2 = sol(input)
let result = Belt.Array.map(arrCase, sol2) -> Belt.Array.reduce(1, (acc, x) => acc * x)

// let result = sol(input, {hPoint : 3, vPoint : 1})

result -> Js.log
