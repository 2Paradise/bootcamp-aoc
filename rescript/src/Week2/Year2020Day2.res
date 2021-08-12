// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day2Input.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day2Input.txt")

/*
part1 
  포함되어야 할 개수의 최대 최소 이므로
  1 - 3 은 문자 포함 개수 카운팅 후 1 <= x <= 3 가 참인 문자누적 카운팅
part2
  둘중 하나의 조건만 만족
  문자 한글자씩 배열 -> index 확인
*/

type policy = (int, int, string, string)

// int 값 변환
let getIntCus = (arr, idx) => arr->Belt.Array.getExn(idx)->Belt.Int.fromString->Belt.Option.getExn

// policy 정보 및 target string 파싱
let parsePolicy = x => {
  let arr = x->Js.String2.split(":")
  let target = arr->Belt.Array.getExn(1)->Js.String2.trim
  let arrPolicy = arr->Belt.Array.getExn(0)->Js.String2.split(" ")
  let arrNum = arrPolicy->Belt.Array.getExn(0)->Js.String2.split("-")
  let str = arrPolicy->Belt.Array.getExn(1)

  [(arrNum->getIntCus(0), arrNum->getIntCus(1), str, target)]
}

// part1 같은 문자 포함 카운트 reduce 후 누적값 min max 비교 1,0 리턴
let checkPart1 = x => {=
  let (min, max, str, target) = x
  let count =
    target->Js.String2.split("")->Belt.Array.reduce(0, (acc, x) => x === str ? acc + 1 : acc)
  (min, max, count)
}

// min, max 해당 문자 index 추출후 비교 및 하나만 일치 조건 1,0 리턴
let checkPart2 = x => {
  let (min, max, str, target) = x
  let arrTarget = target->Js.String2.split("")
  (str, arrTarget->Belt.Array.get(min - 1), arrTarget->Belt.Array.get(max - 1))
}

let checkPart2Valid = ((str, minOp, maxOp)) => {
  switch (minOp, maxOp) {
  | (Some(min), Some(max)) => (min === str || max === str) && !(min === str && max === str) ? 1 : 0
  | _ => 0
  }
}

let data =
  input
  ->Js.String2.split("\n")
  ->Belt.Array.reduce(([]: array<policy>), (acc, x) => acc->Belt.Array.concat(x->parsePolicy))

"Day2 Part1 : "->Js.log
let resultPart1 =
  data
  ->Belt.Array.map(checkPart1)
  ->Belt.Array.reduce(0, (acc, (min, max, cnt)) => min <= cnt && cnt <= max ? acc + 1 : acc)
  ->Js.log

"Day2 Part2 : "->Js.log
let resultPart2 =
  data
  ->Belt.Array.map(checkPart2)
  ->Belt.Array.reduce(0, (acc, x) => acc + x->checkPart2Valid)
  ->Js.log
