let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day9Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day9Input.sample.txt")

let data =
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Belt.Int.fromString)
  ->Belt.Array.keepMap(x => x)

let preambleSize = 25

let arrySum = arr => {
  let res = arr->Belt.Array.map(a =>
    arr->Belt.Array.map(b =>
      switch a !== b {
      | true => (Some(a + b), Some(a), Some(b))
      | false => (None, None, None)
      }
    )
  )
  res->Belt.Array.concatMany
}

let rec runPart1 = start => {
  let arrPreamble = data->Belt.Array.slice(~offset=start - preambleSize, ~len=preambleSize)

  let target = data->Belt.Array.get(start)

  let isPass = arrPreamble->arrySum->Belt.Array.some(((x, _, _)) => x === target)
  if isPass {
    runPart1(start + 1)
  } else {
    target
  }
}

let findWeaknessIndex = ((a: option<int>, b: option<int>)) => {
  let minIndex = a->Belt.Option.map(x => data->Js.Array2.indexOf(x))
  let maxIndex = b->Belt.Option.map(x => data->Js.Array2.indexOf(x))
  (minIndex, maxIndex)
}

let arrIndexNumbers = ((a: option<int>, b: option<int>)) =>
  switch (a, b) {
  | (Some(a), Some(b)) => {
      let arr = data->Belt.Array.slice(~offset=a, ~len=b - a + 1)
      arr->Js.log
      arr
    }
  | _ => []
  }

let sum = x => {
  let res = x->Belt.Array.reduce(0, (acc, x) => acc + x)
  res->Js.log
  res
}

// let rec runPart2 = (weakeness, start) => {
// let arrPreamble = data->Belt.Array.slice(~offset=start - preambleSize, ~len=preambleSize)

// let target = data->Belt.Array.get(start)
// "--- target ---"->Js.log
// target->Js.log
// let isPass =
//   arrPreamble
//   ->arrySum
//   ->Belt.Array.keep(((x, _, _)) => x === target)
//   ->Belt.Array.map(((_, a, b)) => a < b ? (a, b) : (b, a))
//   ->Belt.Array.map(findWeaknessIndex)
//   ->Belt.Array.map(arrIndexNumbers)
//   ->Belt.Array.map(sum)
//   ->Belt.Array.some(x => Some(x) === weakeness)

// isPass->Js.log
// if isPass {
//   target
// } else {
//   // runPart2(weakeness, start + 1)
//   target
// }
//   0
// }

"Day 9 result part 1 :: "->Js.log
runPart1(preambleSize)->Js.log
"Day 9 result part 2 :: "->Js.log
// runPart1(preambleSize)->runPart2(preambleSize)->Js.log
