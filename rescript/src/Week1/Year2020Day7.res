let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.sample.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.txt") // 164
// let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Day7Input.sample2.txt") //

type rec treeNode = {
  name: string,
  cnt: int,
  nodes: array<option<treeNode>>,
}

let removeStr = x => x->Js.String2.replaceByRe(%re("/bags|bag|[0-9]|\./g"), "")->Js.String2.trim
let arrInput = input->Js.String2.split("\n")
let getList = key =>
  arrInput
  ->Belt.Array.keep(x => x->Js.String2.indexOf(key) > -1)
  ->Belt.Array.getExn(0)
  ->Js.String2.split("contain")

let parseContain = x => {
  switch x->Js.String2.trim->Belt.Int.fromString {
  | Some(cnt) =>
    Some({
      name: x->removeStr->Js.String2.trim,
      cnt: cnt,
      nodes: [],
    })
  | None => None
  }
}

// let rec setTree = (node: treeNode) => {
//   let key = node.name ++ " bags contain"

//   let contain =
//     getList(key)
//     ->Belt.Array.getExn(1)
//     ->Js.String2.split(",")
//     ->Js.Array2.map(parseContain)
//     ->Belt.Array.reduce([], (acc, x) => {
//       switch x {
//       | Some(x) => acc->Belt.Array.concat(x->setTree)
//       | None => acc
//       }
//     })

//   // contain -> Js.log

//   // {
//   //     name : node.name
//   //     , cnt : node.cnt
//   //     , nodes : contain
//   // }
// }

// let tree = setTree({name: "shiny gold", cnt: 1, nodes: []})->Js.log
