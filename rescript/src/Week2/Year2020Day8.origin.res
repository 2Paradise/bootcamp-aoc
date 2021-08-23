let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day8Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day8Input.sample.txt")

type oper = Acc(string) | Jmp(string) | Nop(string) | Not

type operation = {
  isRun: bool,
  operType: oper,
  isLast: bool,
  isCorrupted: bool,
}

let initOperation = {
  isRun: false,
  operType: Not,
  isLast: false,
  isCorrupted: false,
}

let arrInput = input->Js.String2.split("\n")

let parseInitRecord = x => {
  switch x->Js.String2.split(" ") {
  | ["acc", v] => {...initOperation, operType: Acc(v)}->Some
  | ["jmp", v] => {...initOperation, operType: Jmp(v)}->Some
  | ["nop", v] => {...initOperation, operType: Nop(v)}->Some
  | _ => None
  }
}

let parseOperation = x => {
  let accOperation = x->Js.String2.replaceByRe(%re("/\d/g"), "")->Some
  let accValue = x->Js.String2.replaceByRe(%re("/\D/g"), "")->Belt.Int.fromString
  (accOperation, accValue)
}

let runAcc = (x, acc) => {
  switch x->parseOperation {
  | (Some("+"), Some(v)) => acc + v
  | (Some("-"), Some(v)) => acc - v
  | _ => acc
  }
}

let runJump = (x, i) => {
  switch x->parseOperation {
  | (Some("+"), Some(v)) => i + v
  | (Some("-"), Some(v)) => i - v
  | _ => i
  }
}

let checkRun = (x, idx) => x->Belt.Array.mapWithIndex((i, x) => idx === i ? {...x, isRun: true} : x)

let getExcuteAccAndIndex = (operType, acc, i) => {
  switch operType {
  | Acc(v) => (v->runAcc(acc), i + 1)
  | Jmp(v) => (acc, v->runJump(i))
  | Nop(_) => (acc, i + 1)
  | _ => (acc, i + 1)
  }
}

let rec runPart1Excute = (x, (acc, i)) => {
  let {isRun, operType} = x[i]
  switch isRun {
  | false => x->checkRun(i)->runPart1Excute(getExcuteAccAndIndex(operType, acc, i))
  | true => acc
  }
}

let arrExcute = arrInput->Belt.Array.map(parseInitRecord)->Belt.Array.keepMap(x => x)

"Day 8 part1 result :: "->Js.log
arrExcute->runPart1Excute((0, 0))->Js.log

let lenExcute = arrExcute->Belt.Array.length

let checkLast = x =>
  x->Belt.Array.mapWithIndex((i, x) => lenExcute - 1 === i ? {...x, isLast: true} : x)

let checkCorrupted = (x, idx) =>
  x->Belt.Array.mapWithIndex((i, x) => idx === i ? {...x, isCorrupted: true} : x)

let arrPart2Excute = arrExcute->checkLast

let arrCorruptedIndex =
  arrExcute
  ->Belt.Array.mapWithIndex((i, x): option<int> => {
    switch x.operType {
    | Acc(_) => None
    | _ => Some(i)
    }
  })
  ->Belt.Array.keepMap(x => x)

let changeOperType = x =>
  switch x {
  | Jmp(v) => Nop(v)
  | Nop(v) => Jmp(v)
  | _ => Not
  }

let rec runPart2Excute = (x, (acc, i)) => {
  let {isRun, operType, isCorrupted, isLast} = x[i]

  let newOperType = isCorrupted ? operType->changeOperType : operType

  switch isRun {
  | false =>
    if isLast {
      let (acc, _) = getExcuteAccAndIndex(newOperType, acc, i)
      (acc, isLast)
    } else {
      x->checkRun(i)->runPart2Excute(getExcuteAccAndIndex(newOperType, acc, i))
    }
  | true => (acc, isLast)
  }
}

let runExcute = (arrCorruptedIndex, arrPart2Excute) => {
  arrCorruptedIndex
  ->Belt.Array.map(x => arrPart2Excute->checkCorrupted(x))
  ->Belt.Array.map(x => x->runPart2Excute((0, 0)))
}

"Day 8 part2 result :: "->Js.log
let resultPart2 =
  arrCorruptedIndex
  ->runExcute(arrPart2Excute)
  ->Belt.Array.keep(((_, isLast)) => isLast)
  ->Belt.Array.get(0)

resultPart2->Js.log
