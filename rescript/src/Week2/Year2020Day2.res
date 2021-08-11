let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day2Input.sample.txt")

/*
포함되어야 할 개수의 최대 최소 이므로 
1 - 3 은 문자 포함 개수 카운팅 후 1 <= x <= 3 가 참인 문자누적 카운팅
*/

type policy = (int, int, string, string)

let getInt = (arr, idx) => arr->Belt.Array.getExn(idx)->Belt.Int.fromString->Belt.Option.getExn

let parsePolicy = x => {
  let arr = x->Js.String2.split(":")
  let policy = arr->Belt.Array.getExn(0)
  let target = arr->Belt.Array.getExn(1)->Js.String2.trim
  let arrPolicy = policy->Js.String2.split(" ")
  let arrNum = arrPolicy->Belt.Array.getExn(0)->Js.String2.split("-")
  let str = arrPolicy->Belt.Array.getExn(1)

  [(arrNum->getInt(0), arrNum->getInt(1), str, target)]
}

let checkValid = x => {
  //   x->Js.log
  //   let (min, max, str, target) = x
  //   let parseRe = str->Js.Re.fromString
  //   let reStr = `/${parseRe}/g`
  //   reStr->Js.log
  //   target |> Js.String.match_(%re(reStr))->Js.log
  1
}

"part1 result :: "->Js.log

input
->Js.String2.split("\n")
->Belt.Array.reduce(([]: array<policy>), (acc, x) => acc->Belt.Array.concat(x->parsePolicy))
->Belt.Array.reduce(0, (acc, x) => acc + x->checkValid)
->Js.log
