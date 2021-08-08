// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day5Input.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day5Input.txt")

type move = 
| Row(string)
| Col(string)

type seat =
| Lower
| Upper

type arrSeat = array<(int, int)>

let arrInput = input -> Js.String2.split("\n")

let convertLU = (x) => x 
-> Js.String2.replaceByRe(%re("/[F|L]/g"), "0") 
-> Js.String2.replaceByRe(%re("/[B|R]/g"), "1")

let getRowInput = (x) => Js.String2.slice(x, ~from=0, ~to_=7) -> convertLU
let getColInput = (x) => Js.String2.sliceToEnd(x, ~from=7) -> convertLU
let splitPart = (x) => ( x -> getRowInput -> Row, x -> getColInput -> Col )

let calcSeatNum = (x, (a, b)) => {
    x -> Js.String2.split("")
    -> Belt.Array.map(x => x -> Belt.Int.fromString -> Belt.Option.getExn > 0 ? Upper : Lower) 
    -> Belt.Array.reduce((a, b), ((front, end), x) => {
        let calc = (end - front)/2
        switch x {
        | Lower => (front, end - calc - 1)
        | Upper => (front + calc + 1, end)
        }
    }) 
    -> ((a,b)) => a < b ? a : b
}

let getSeat = (x) => {
    switch x {
    | Row(str) => str -> calcSeatNum((0, 127))
    | Col(str) => str -> calcSeatNum((0, 7))
    }
}

let getSeatNumber = ((row, col)) => (row -> getSeat, col -> getSeat)

let geId = ((row, col)) => row * 8 + col

let arrSeatId = arrInput 
-> Belt.Array.map(splitPart) 
-> Belt.Array.map(getSeatNumber)
-> Belt.Array.map(geId)

let resultPart1 = arrSeatId
-> Belt.Array.reduce(0, (acc, x) => acc > x ? acc : x)

let arrCheckId = arrSeatId
-> Js.Array2.sortInPlaceWith((a, b) => a - b)

let resultPart2 = arrCheckId -> Belt.Array.keepWithIndex((x, idx) => {
    
    idx === Js.Array.length(arrCheckId) - 1 ? false : {
        switch Some(arrCheckId[idx+1]) {
        | Some(nextInt) => nextInt - x > 1
        | _ => false
        }
    }

})

"result : part 1" -> Js.log
resultPart1 -> Js.log

"result : part 2" -> Js.log
(resultPart2[0] + 1) -> Js.log
