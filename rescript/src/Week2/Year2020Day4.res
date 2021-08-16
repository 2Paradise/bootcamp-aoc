let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample1.txt")
// let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Day4Input.sample2.txt")

// --- Day 4: Passport Processing ---
// part1

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/

type eyeClolr = [#amb | #blu | #brn | #gry | #grn | #hzl | #oth]

type valid =
  | Byr(string)
  | Iyr(string)
  | Eyr(string)
  | HgtCm(string)
  | HgtIn(string)
  | Hcl(string)
  | Ecl(eyeClolr)
  | Pid(string)
  | None

type passport = {
  byr: valid,
  iyr: valid,
  eyr: valid,
  hgt: valid,
  hcl: valid,
  ecl: valid,
  pid: valid,
}

let initPassport = {
  byr: None,
  iyr: None,
  eyr: None,
  hgt: None,
  hcl: None,
  ecl: None,
  pid: None,
}
/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/

let parseRecord = x => {
  x->Belt.Array.reduce(initPassport, (acc, x) => {
    switch x->Js.String2.split(":") {
    | ["byr", value] => {...acc, byr: Byr(value)}
    | ["iyr", value] => {...acc, iyr: Iyr(value)}
    | ["eyr", value] => {...acc, eyr: Eyr(value)}
    | ["hgt", value] => {
        let hgtType = switch value->Js.String2.replaceByRe(%re("/\d/g"), "") {
        | "cm" => HgtCm(value)
        | "in" => HgtIn(value)
        | _ => None
        }
        {...acc, hgt: hgtType}
      }
    | ["hcl", value] => {...acc, hcl: Hcl(value)}
    | ["ecl", value] => {
        let eclType = switch value {
        | "amb" => Ecl(#amb)
        | "blu" => Ecl(#blu)
        | "brn" => Ecl(#brn)
        | "gry" => Ecl(#gry)
        | "grn" => Ecl(#grn)
        | "hzl" => Ecl(#hzl)
        | "oth" => Ecl(#oth)
        | _ => None
        }
        {...acc, ecl: eclType}
      }
    | ["pid", value] => {...acc, pid: Pid(value)}
    | _ => acc
    }
  })
}

let parsePassport = (acc, x) => acc->Belt.Array.concat([x->parseRecord])

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let isNotEmpty = x => {
  switch x {
  | Byr(_) => true
  | Iyr(_) => true
  | Eyr(_) => true
  | HgtCm(_) => true
  | HgtIn(_) => true
  | Hcl(_) => true
  | Ecl(_) => true
  | Pid(_) => true
  | None => false
  }
}

let countPassport: array<passport> => int = x => {
  x->Belt.Array.reduce(0, (acc, x) => {
    let {byr, iyr, eyr, hgt, hcl, ecl, pid} = x

    switch (
      byr->isNotEmpty,
      iyr->isNotEmpty,
      eyr->isNotEmpty,
      hgt->isNotEmpty,
      hcl->isNotEmpty,
      ecl->isNotEmpty,
      pid->isNotEmpty,
    ) {
    | (true, true, true, true, true, true, true) => acc + 1
    | _ => acc
    }
  })
}

let splitPassPort = x => x->Js.String2.replaceByRe(%re("/\\n/g"), " ")->Js.String2.split(" ")

let arrInput = input->Js.String2.split("\n\n")

/*
part 1 result 233 !
part1 count part2 count가 다르다. 키 cm, in 포함 문자가 유효하다하는데 
cm || in 가 포함되어 있지 않은 정보가 있음 [hgt]
*/
"Day 4 Part 1 ::"->Js.log
let arrPassport =
  arrInput->Belt.Array.map(splitPassPort)->Belt.Array.reduce(([]: array<passport>), parsePassport)

arrPassport->countPassport->Js.log

"Day 4 Part 2 ::"->Js.log

let checkRangeType = (x, (a: int, b: int)) => {
  let value = x->Belt.Int.fromString->Belt.Option.getExn
  a <= value && value <= b
}

let checkLength = (x, len) => x->Js.String2.length === len

let checkHex = x => x |> Js.Re.test_(%re("/^[\#][0-9a-f]{6}$/"))

let checkEyeColor = x =>
  switch x {
  | #...eyeClolr => true
  | _ => false
  }

let passportValid = x => {
  switch x {
  | Byr(v) => v->checkRangeType((1920, 2002)) && v->checkLength(4)
  | Iyr(v) => v->checkRangeType((2010, 2020)) && v->checkLength(4)
  | Eyr(v) => v->checkRangeType((2020, 2030)) && v->checkLength(4)
  | HgtCm(v) => v->checkRangeType((150, 193))
  | HgtIn(v) => v->checkRangeType((59, 76))
  | Hcl(v) => v->checkHex
  | Ecl(v) => v->checkEyeColor
  | Pid(v) => v->checkLength(9)
  | None => false
  }
}

let validatePassports: array<passport> => int = x => {
  x->Belt.Array.reduce(0, (acc, x) => {
    let {byr, iyr, eyr, hgt, hcl, ecl, pid} = x
    switch (
      byr->passportValid,
      iyr->passportValid,
      eyr->passportValid,
      hgt->passportValid,
      hcl->passportValid,
      ecl->passportValid,
      pid->passportValid,
    ) {
    | (true, true, true, true, true, true, true) => acc + 1
    | _ => acc
    }
  })
}

arrPassport->validatePassports->Js.log

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
