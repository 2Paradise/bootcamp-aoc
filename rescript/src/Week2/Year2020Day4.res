let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample1.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample2.txt")

// --- Day 4: Passport Processing ---
// part1

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/

type eyeClolr = [#amb | #blu | #brn | #gry | #grn | #hzl | #oth | #none]

type htg = Cm(int) | In(int)

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: htg,
  hcl: string,
  ecl: eyeClolr,
  pid: string,
}

let initPassport = {
  byr: 0,
  iyr: 0,
  eyr: 0,
  hgt: Cm(0),
  hcl: "",
  ecl: #none,
  pid: "",
}

/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/
let parseStringToInt = x =>
  switch x->Belt.Int.fromString {
  | Some(i) => i
  | _ => 0
  }

// array<Js.String2.t> => option<passport
let parseRecord = x => {
  x->Belt.Array.reduce(Some(initPassport), (acc, x) => {
    Some(x)->Belt.Option.map(x => {
      switch acc {
      | Some(acc) =>
        switch x->Js.String2.split(":") {
        | ["byr", v] => {...acc, byr: v->parseStringToInt}
        | ["iyr", v] => {...acc, iyr: v->parseStringToInt}
        | ["eyr", v] => {...acc, eyr: v->parseStringToInt}
        | ["hgt", v] => {
            let hgtType = switch v->Js.String2.replaceByRe(%re("/\d/g"), "") {
            | "cm" => Cm(v->parseStringToInt)
            | "in" => In(v->parseStringToInt)
            | _ => Cm(0)
            }
            {...acc, hgt: hgtType}
          }
        | ["hcl", v] => {...acc, hcl: v}
        | ["ecl", v] => {
            let eclType = switch v {
            | "amb" => #amb
            | "blu" => #blu
            | "brn" => #brn
            | "gry" => #gry
            | "grn" => #grn
            | "hzl" => #hzl
            | "oth" => #oth
            | _ => #none
            }
            {...acc, ecl: eclType}
          }
        | ["pid", v] => {...acc, pid: v}
        | _ => acc
        }
      | _ => initPassport
      }
    })
  })
}

let parsePassport = x => x->parseRecord

let sumInt = arr => arr->Belt.Array.reduce(0, (acc, x) => acc + x)

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let passPortCheck = x => {
  x->Belt.Option.map(({byr, iyr, eyr, hgt, hcl, ecl, pid}) =>
    byr !== 0 &&
    iyr !== 0 &&
    eyr !== 0 &&
    hgt !== Cm(0) &&
    hcl !== "" &&
    ecl !== #none &&
    pid !== ""
      ? 1
      : 0
  )
}

let arrInput = input->Js.String2.split("\n\n")

let splitPassPort = x => x->Js.String2.replaceByRe(%re("/\\n/g"), " ")->Js.String2.split(" ")

/*
part 1 result 233 !
part1 count part2 count가 다르다. 키 cm, in 포함 문자가 유효하다하는데 
cm || in 가 포함되어 있지 않은 정보가 있음 [hgt]
*/

let arrPassport = arrInput->Belt.Array.map(splitPassPort)->Belt.Array.map(parsePassport)

"Day 4 Part 1 ::"->Js.log
arrPassport->Belt.Array.map(passPortCheck)->Belt.Array.keepMap(x => x)->sumInt->Js.log

let checkRangeType = (x, (a, b)) => a <= x && x <= b

let checkLength = (x, len) => x->Js.String2.length === len

let checkHex = x => x |> Js.Re.test_(%re("/^[\#][0-9a-f]{6}$/"))

let checkEyeColor = x =>
  switch x {
  | #none => false
  | #...eyeClolr => true
  }

let checkHgt = x =>
  switch x {
  | Cm(h) => h->checkRangeType((150, 193))
  | In(h) => h->checkRangeType((59, 76))
  }

let passportDiv = x => {
  x->Belt.Option.map(({byr, iyr, eyr, hgt, hcl, ecl, pid}) => {
    switch (
      byr->checkRangeType((1920, 2002)),
      iyr->checkRangeType((2010, 2020)),
      eyr->checkRangeType((2020, 2030)),
      hgt->checkHgt,
      hcl->checkHex,
      ecl->checkEyeColor,
      pid->checkLength(9),
    ) {
    | (true, true, true, true, true, true, true) => 1
    | _ => 0
    }
  })
}
"Day 4 Part 2 ::"->Js.log
arrPassport->Belt.Array.map(passportDiv)->Belt.Array.keepMap(x => x)->sumInt->Js.log
