// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.txt")

type bagInfo = 
{
    color : string
    , isContain : bool
}

let removeStr = (x) => x -> Js.String2.replaceByRe(%re("/bags|bag|\.|[0-9]/g"), "") -> Js.String2.trim
let arrInput = input -> Js.String2.split("\n") -> Js.Array2.map(x => x -> Js.String2.split("contain") -> Belt.List.fromArray)
let getContainBags = (x) => x -> Belt.List.getExn(0) -> Js.String2.split(",") -> Js.Array2.map(x => x -> removeStr -> Js.String2.trim)

let rec setBagContainInfo = (list, accContainColor) => {

    let newList = list -> Js.Array2.filter(((a, _)) => !Js.Array2.includes(accContainColor, a))

    let newAccContainColor = newList -> Belt.Array.reduce( accContainColor, (acc, (color, arrContain)) => {
        let isContain = arrContain -> Belt.Array.some(x => Js.Array2.includes(accContainColor, x))
        isContain ? Belt.Array.concat(acc, [color]) : acc
    })
    
    if Js.Array.length(accContainColor) !== Js.Array.length(newAccContainColor) {
        setBagContainInfo(newList, newAccContainColor)
    }
    else {
        Js.Array.length(newAccContainColor) - 1
    }
}

// part 1 solution
arrInput -> Belt.Array.reduce([], (acc, x) => {
    switch x {
    | list{} => acc
    | list{bag, ...arrContainBagList} => Belt.Array.concat(acc, [(bag -> removeStr, arrContainBagList -> getContainBags)])
    }
}) -> setBagContainInfo(["shiny gold"]) -> Js.log