// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day2Input.sample.txt")
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day2Input.txt")

let sum = arr => arr->Belt.Array.reduce(0, (acc, x) => acc + x)

/*
part1 
  포함되어야 할 개수의 최대 최소 이므로
  1 - 3 은 문자 포함 개수 카운팅 후 1 <= x <= 3 가 참인 문자누적 카운팅
part2
  둘중 하나의 조건만 만족
  문자 한글자씩 배열 -> index 확인
*/

type policy = (int, int, string, string)

// map :: (a -> b) -> Option<a> -> Option<b>
// flatMap :: (a -> Option<b>) -> Option<a> -> Option<b>

// int 값 변환
let getIntCus = (arr, idx) => arr->Belt.Array.get(idx)->Belt.Option.flatMap(Belt.Int.fromString)

// policy 정보 및 target string 파싱
// (option<int>, option<int>, string, string)
// option<(int, int, string, string))

let parsePolicy = x => {
  let arr = x->Js.String2.split(":")

  let target = arr->Belt.Array.get(1)->Belt.Option.flatMap(x => Some(x->Js.String2.trim))

  let arrPolicy = switch arr->Belt.Array.get(0) {
  | Some(arr) => Some(arr->Js.String2.split(" "))
  | _ => None
  }

  let (arrNum, str) = switch arrPolicy {
  | Some(arr) => {
      let arrNum = arr->Belt.Array.get(0)->Belt.Option.flatMap(x => Some(x->Js.String2.split("-")))
      let str = arr->Belt.Array.get(1)
      (arrNum, str)
    }
  | _ => (None, None)
  }

  switch arrNum {
  | Some(arr) =>
    switch (arr->getIntCus(0), arr->getIntCus(1), str, target) {
    | (Some(i0), Some(i1), Some(str), Some(target)) => Some((i0, i1, str, target))
    | _ => None
    }
  | _ => None
  }
}

// part1 같은 문자 포함 카운트 reduce 후 누적값 min max 비교 1,0 리턴
let checkPart1 = x => {
  let (min, max, str, target) = x
  let count = target->Js.String2.split("")->Belt.Array.map(x => x === str ? 1 : 0)->sum

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

let data = input->Js.String2.split("\n")->Belt.Array.map(parsePolicy)

"Day2 Part1 : "->Js.log
let resultPart1 =
  data
  ->Belt.Array.keepMap(x => x)
  ->Belt.Array.map(checkPart1)
  ->Belt.Array.map(((min, max, cnt)) => min <= cnt && cnt <= max ? 1 : 0)
  ->sum
  ->Js.log

"Day2 Part2 : "->Js.log
let resultPart2 =
  data
  ->Belt.Array.keepMap(x => x)
  ->Belt.Array.map(checkPart2)
  ->Belt.Array.map(x => x->checkPart2Valid)
  ->sum
  ->Js.log
