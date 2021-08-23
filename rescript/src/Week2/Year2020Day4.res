let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample1.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample2.txt")

let sumInt = arr => arr->Belt.Array.reduce(0, (acc, x) => acc + x)

let arrInput = input->Js.String2.split("\n\n")

let splitPassPort = x => x->Js.String2.replaceByRe(%re("/\\n/g"), " ")

let checkPart = x => {
  let passportRe = %re("/(?=.*byr)(?=.*iyr)(?=.*eyr)(?=.*hgt)(?=.*hcl)(?=.*ecl)(?=.*pid)/")
  switch x->Js.String2.match_(passportRe) {
  | Some(_) => x->Some
  | None => None
  }
}

"Day 4 Part 1 ::"->Js.log
arrInput->Belt.Array.map(splitPassPort)->Belt.Array.keepMap(checkPart)->Belt.Array.length->Js.log

"Day 4 Part 2 ::"->Js.log

type eyeColor = [#amb | #blu | #brn | #gry | #grn | #hzl | #oth]

type htg = Cm(int) | In(int)

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: htg,
  hcl: string,
  ecl: eyeColor,
  pid: string,
  cid: option<string>,
}

let splitFields = x =>
  x->Belt.Array.keepMap(x =>
    switch x->Js.String2.split(":") {
    | [k, v] => (k, v)->Some
    | _ => None
    }
  )

module Check = {
  let parseStringToInt = x =>
    switch x->Belt.Int.fromString {
    | Some(i) => i
    | _ => 0
    }

  let rangeType = (x, (a, b)) =>
    x
    ->Belt.Option.flatMap(((_, v)) => v->Belt.Int.fromString)
    ->Belt.Option.flatMap(x => a <= x && x <= b ? Some(x) : None)

  let hex = x =>
    x->Belt.Option.flatMap(((_, v)) =>
      v |> Js.Re.test_(%re("/^[\#][0-9a-f]{6}$/")) ? Some(v) : None
    )

  let length = (x, len) =>
    x->Belt.Option.flatMap(((_, v)) => v->Js.String2.length === len ? Some(v) : None)

  let hgt = x => {
    x->Belt.Option.flatMap(((_, v)) =>
      switch v->Js.String2.replaceByRe(%re("/\d/g"), "") {
      | "cm" => x->rangeType((150, 193))->Belt.Option.map(x => Cm(x))
      | "in" => x->rangeType((59, 76))->Belt.Option.map(x => In(x))
      | _ => None
      }
    )
  }

  let eyeColor = x => {
    x->Belt.Option.flatMap(((_, v)) =>
      switch v {
      | "amb" => #amb->Some
      | "blu" => #blu->Some
      | "brn" => #brn->Some
      | "gry" => #gry->Some
      | "grn" => #grn->Some
      | "hzl" => #hzl->Some
      | "oth" => #oth->Some
      | _ => None
      }
    )
  }
}

let getKeyValue = (x, key) => x->Belt.Array.keep(((k, _)) => k === key)->Belt.Array.get(0)

let parse = x => {
  let byr = x->getKeyValue("byr")->Check.rangeType((1920, 2002))
  let iyr = x->getKeyValue("iyr")->Check.rangeType((2010, 2020))
  let eyr = x->getKeyValue("eyr")->Check.rangeType((2020, 2030))
  let hgt = x->getKeyValue("hgt")->Check.hgt
  let hcl = x->getKeyValue("hcl")->Check.hex
  let ecl = x->getKeyValue("ecl")->Check.eyeColor
  let pid = x->getKeyValue("pid")->Check.length(9)
  let cid = x->getKeyValue("cid")->Belt.Option.map(((_, v)) => v)

  switch (byr, iyr, eyr, hgt, hcl, ecl, pid) {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid)) =>
    {
      byr: byr,
      iyr: iyr,
      eyr: eyr,
      hgt: hgt,
      hcl: hcl,
      ecl: ecl,
      pid: pid,
      cid: cid,
    }->Some
  | _ => None
  }
}

arrInput
->Belt.Array.map(splitPassPort)
->Belt.Array.keepMap(checkPart)
->Belt.Array.map(x => x->Js.String2.split(" "))
->Belt.Array.map(splitFields)
->Belt.Array.keepMap(parse)
->Belt.Array.length
->Js.log
