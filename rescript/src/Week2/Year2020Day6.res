let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day6Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day6Input.sample.txt")

/*
  part1 그룹당 질문 중복 제거 후 개수 합

  - 그룹 나누기
  - 그룹 질문 array -> set -> 중복제거
  - 그룹당 질문 개수
  - 그룹 질분 개수 합
*/

let sumInt = arr => arr->Belt.Array.reduce(0, (acc, x) => acc + x)

let arrGroup = input->Js.String2.split("\n\n")

let splitCharAt = x => x->Js.String2.replaceByRe(%re("/\\n/g"), "")->Js.String2.split("")

let duplArray = x => x->Belt.Set.String.fromArray->Belt.Set.String.toArray

let getLength = x => x->Belt.Array.length

let resultPart1 =
  arrGroup
  ->Belt.Array.map(splitCharAt)
  ->Belt.Array.map(duplArray)
  ->Belt.Array.map(getLength)
  ->sumInt

"part1 result :: "->Js.log
resultPart1->Js.log

/*
  그룹당 공통된 Yes 질문 카운터. 완벽하게 공통된 질문만 Yes (1명 이상일 경우)
  Belt.SetString.intersect 활용
*/
let splitMember = x => x->Js.String2.split("\n")

let checkAnser = x =>
  x->Belt.Array.reduce(x[0], (acc, x) => {
    let s0 = acc->Belt.Set.String.fromArray
    let s1 = x->Belt.Set.String.fromArray
    Belt.Set.String.intersect(s0, s1)->Belt.Set.String.toArray
  })

let checkCommonAnser = x =>
  x->Belt.Array.map(x => x->Js.String2.split(""))->checkAnser->Belt.Array.length

let resultPart2 = arrGroup->Belt.Array.map(splitMember)->Belt.Array.map(checkCommonAnser)->sumInt

resultPart2->Js.log
