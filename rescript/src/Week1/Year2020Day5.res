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

let resultPart1 = arrInput -> Belt.Array.map(splitPart) -> Belt.Array.reduce(0, (acc, x) => {
    let result = x -> getSeatNumber -> geId
    acc > result ? acc : result
})

let resultPart2 = arrInput 
-> Belt.Array.map(splitPart) 
-> Belt.Array.reduce( [] : arrSeat , (acc, x) => Js.Array2.concat(acc, [x -> getSeatNumber]))
-> Js.Array2.filter(((row, _)) => row !== 0 || row !== 127 )


"result : part 1" -> Js.log
resultPart1 -> Js.log

"result : part 2" -> Js.log
// resultPart2 -> Js.log


// "000" -> Col -> getSeat -> Js.log
