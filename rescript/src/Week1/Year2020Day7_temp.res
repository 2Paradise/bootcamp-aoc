let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.sample.txt") 
// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.txt") // 164

type containBag = (int, option<string>)
type bagType = (string, array<containBag>)

let removeStr = (x) => x -> Js.String2.replaceByRe(%re("/bags|bag|[0-9]|\./g"), "") -> Js.String2.trim
let arrInput = input -> Js.String2.split("\n")
let splitInfo = (x) => x -> Js.String2.split("contain") -> Belt.List.fromArray

let parseContainInfo = (x) => {
    switch x -> Js.String2.trim -> Belt.Int.fromString {
    | Some(cnt) => (cnt, Some(x -> removeStr -> Js.String2.trim))
    | None => (0, None)
    }
}

let getContainBags = (x) => x -> Belt.List.getExn(0) -> Js.String2.split(",") -> Js.Array2.map(parseContainInfo)

let rec getBagCount = (list : array<bagType>, accContainColor) => {

    let newList = list -> Js.Array2.filter(((a, _)) => !Js.Array2.includes(accContainColor, a))

    let newAccContainColor = newList -> Belt.Array.reduce( accContainColor, (acc, (color, arrContain)) => {
        
        let isContain = arrContain -> Belt.Array.some(((_, x)) => {
            switch x {
            | Some(x) => Js.Array2.includes(accContainColor, x)
            | None => false
            }
        })
        
        isContain ? Belt.Array.concat(acc, [color]) : acc
    })
    
    if Js.Array.length(accContainColor) !== Js.Array.length(newAccContainColor) {
        getBagCount(newList, newAccContainColor)
    }
    else {
        Js.Array.length(newAccContainColor) - 1
    }
}

// let rec getBagContainCount = (list : array<bagType>, containBagInfo) => {

//     containBagInfo -> Js.log
//     let ( bagCnt, containBagName ) = containBagInfo

//     switch containBagName {
//     | Some(containBagName) => {
//         let containCnt = list
//         -> Belt.Array.keep((( bag, _)) => containBagName === bag)
//         -> Belt.Array.map(((_, x)) => x)
//         -> Belt.Array.reduce( 0, (accCnt, x) => accCnt + x -> Belt.Array.reduce(0, (acc1, (cnt1 , _)) => acc1 + cnt1))

//         bagCnt*containCnt
//     }
//     | None => 0
//     }

// }

let getBagInfoList = arrInput
 -> Js.Array2.map(splitInfo)
 -> Belt.Array.reduce([] : array<bagType>, (acc, x) => {
    switch x {
    | list{bag, ...arrContainBagList} => Belt.Array.concat(acc, [(bag -> removeStr, arrContainBagList -> getContainBags)])
    | list{} => acc
    }
})

// part 1 solution
// "part 1 solution" -> Js.log
// getBagInfoList -> getBagCount(["shiny gold"]) -> Js.log

// part 2 solution
"part 2 solution" -> Js.log
// getBagInfoList -> getBagContainCount(0, (1, Some("shiny gold"))) -> Js.log
let getTargetList = (list, target: containBag, accTargetList) => {

    let ( _ , containBagName ) = target

    switch containBagName {
    | Some(containBagName) => {

        list
        -> Belt.Array.keep((( bag, _)) => containBagName === bag)
        -> Belt.Array.map(((_, x)) => x) 
        -> Belt.Array.reduce(accTargetList, (acc, x) => {
            
            x -> Belt.Array.reduce([], (acc, x) => {
                x -> Js.log
                acc
            }) -> Js.log

            acc
        }) -> Js.log

        accTargetList
    }
    | None => accTargetList
    }
}

getBagInfoList -> getTargetList((1, Some("shiny gold")), []) -> Js.log