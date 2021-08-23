let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day8Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day8Input.sample.txt")

// 실행된 정보, oper type, oper value // acc
type oper = Acc(string) | Jmp(string) | Nop(string)
type opers = array<oper>

// let run = (acc, opers, index) => {
//   let x = opers[index]

//   switch x {
//   | Acc(i) => (acc + i, index)
//   | Jmp(i) => (acc, index + i)
//   | Nop => (acc, index + 1)
//   }
// }

// run(0, opers, 0)
let arrInput = input->Js.String2.split("\n")

let parseInitRecord = x => {
  switch x->Js.String2.split(" ") {
  | ["acc", v] => Acc(v)->Some
  | ["jmp", v] => Jmp(v)->Some
  | ["nop", v] => Nop(v)->Some
  | _ => None
  }
}

let parseOperation = x => {
  let accOperation = x->Js.String2.replaceByRe(%re("/\d/g"), "")->Some
  let accValue = x->Js.String2.replaceByRe(%re("/\D/g"), "")->Belt.Int.fromString
  (accOperation, accValue)
}

let runExcute = (x, t) => {
  switch x->parseOperation {
  | (Some("+"), Some(v)) => t + v
  | (Some("-"), Some(v)) => t - v
  | _ => t
  }
}

let getExcuteResult = (operType, acc, i) => {
  switch operType {
  | Acc(v) => (v->runExcute(acc), i + 1)
  | Jmp(v) => (acc, v->runExcute(i))
  | Nop(_) => (acc, i + 1)
  }
}

let rec runPart1Excute = (x, arrRunIndex, (acc, i)) => {
  if !(arrRunIndex->Js.Array2.includes(i)) {
    x->runPart1Excute(arrRunIndex->Belt.Array.concat([i]), getExcuteResult(x[i], acc, i))
  } else {
    acc
  }
}

let arrExcute = arrInput->Belt.Array.map(parseInitRecord)->Belt.Array.keepMap(x => x)

"Day 8 part1 result :: "->Js.log
arrExcute->runPart1Excute([], (0, 0))->Js.log

let lenExcute = arrExcute->Belt.Array.length

let arrCorruptedIndex =
  arrExcute
  ->Belt.Array.mapWithIndex((i, x): option<int> => {
    switch x {
    | Acc(_) => None
    | _ => Some(i)
    }
  })
  ->Belt.Array.keepMap(x => x)

let swap = x =>
  switch x {
  | Jmp(v) => Nop(v)
  | Nop(v) => Jmp(v)
  | _ => x
  }

let checkOper = x =>
  switch x {
  | Acc(_) => false
  | _ => true
  }

type loop = Loop | TestLoop

let rec runPart2Excute = (x, loopType, accq, (arrA, arrB), (acc, i)) => {
  let isLast = lenExcute - 1 === i
  let isIncluedes = arrA->Belt.Array.concat(arrB)->Js.Array2.includes(i)
  let isSwap = x[i]->checkOper

  if isLast {
    let (acc, _) = getExcuteResult(x[i], acc, i)
    acc
  } else {
    switch loopType {
    | Loop =>
      switch isIncluedes {
      | false =>
        switch isSwap {
        | true =>
          x->runPart2Excute(
            TestLoop,
            acc,
            (arrA, arrB->Belt.Array.concat([i])),
            getExcuteResult(x[i]->swap, acc, i),
          )
        | false =>
          x->runPart2Excute(
            Loop,
            accq,
            (arrA->Belt.Array.concat([i]), arrB),
            getExcuteResult(x[i], acc, i),
          )
        }
      | true => acc
      }
    | TestLoop =>
      switch isIncluedes {
      | true => {
          let startIdx = arrB->Belt.Array.get(0)
          switch startIdx {
          | Some(si) =>
            x->runPart2Excute(
              Loop,
              accq,
              (arrA->Belt.Array.concat([si]), []),
              getExcuteResult(x[si], accq, si),
            )
          | None => acc
          }
        }
      | false =>
        x->runPart2Excute(
          TestLoop,
          accq,
          (arrA, arrB->Belt.Array.concat([i])),
          getExcuteResult(x[i], acc, i),
        )
      }
    }
  }
}

// 20
// [] -> [0] -> [0, 1] -> [0, 1, 10] -> [0 ,1, 10, 15] -> [0, 1, 10, 15, 3] -> ([0 ,1, 10, 15], 3) -> ([0 ,1, 10], 15)
/*
  ([],[]) -> 
*/

/*
instructions :: array<oper> [Acc(3), Jmp(2), Nop, Nop, Acc(2), Jmp(-3)]
executions :: list<(int, int)> -> instructions index
[(0, 0), (3, 1), (3, 3)]

(prevState, action) -> nextState

run(Nop, (executions)) -> (executions ++ [run index])

*/

"Day 8 part2 result :: "->Js.log
arrExcute->runPart2Excute(Loop, 0, ([], []), (0, 0))->Js.log

let rec runPart3Excute = (x, arrCorruptedIndex, arrIndex, (acc, i)) => {
  let corruptedIndex = arrCorruptedIndex->Belt.Array.get(Belt.Array.length(arrCorruptedIndex) - 1)
  /*
  let oper = switch corruptedIndex->Belt.Option.isSome && corruptedIndex === i->Some {
  | true => x[i]->swap
  | false => x[i]
  }
 */
  let oper = corruptedIndex->Belt.Option.mapWithDefault(x[i], ci => ci == i ? x[i]->swap : x[i])

  if lenExcute - 1 === i {
    "Day 8 part2 - 1 result :: "->Js.log
    let (acc, _) = getExcuteResult(oper, acc, i)
    acc
  } else if !(arrIndex->Js.Array2.includes(i)) {
    x->runPart3Excute(
      arrCorruptedIndex,
      arrIndex->Belt.Array.concat([i]),
      getExcuteResult(oper, acc, i),
    )
  } else {
    x->runPart3Excute(
      arrCorruptedIndex->Belt.Array.keep(x => x->Some !== corruptedIndex),
      [],
      (0, 0),
    )
  }
}

arrExcute->runPart3Excute(arrCorruptedIndex, [], (0, 0))->Js.log
