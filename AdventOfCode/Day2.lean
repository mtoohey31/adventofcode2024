import AdventOfCode.Basic
import AssignOps

def List.pairs : List α → List (α × α)
  | [] => []
  | [_] => []
  | x₀ :: x₁ :: xs => (x₀, x₁) :: (x₁ :: xs).pairs

namespace AdventOfCode

def parse (input : String) :=
  input.splitOn "\n" |>.dropLast.map (·.splitOn " ") |>.mapM (·.mapM (·.toNat?))

def inRange : Nat × Nat → Bool | (x, y) => x > y && x <= y + 3

def part1 (input : String) := parse input <&> List.countP safe
where
  safe (xs : List Nat) := [xs.pairs, xs.reverse.pairs].any (·.all inRange)

#assert part1 with "Day2-example" is some 2

#run part1 with "Day2"

def part2 (input : String) := (parse input <&> List.map List.toArray) <&> List.countP safe
where
  safe (xs : Array Nat) : Bool := [xs, xs.reverse].any safe'
  safe' (xs : Array Nat) : Bool := Id.run do
    let mut oneBad := false
    let mut xs := xs
    let mut i := 1
    repeat
      if i == xs.size then
        break

      let prev := xs[i - 1]!
      let curr := xs[i]!
      if inRange (prev, curr) then
        i += 1
        continue

      if oneBad then
        return false

      oneBad := true
      let some next := xs[i + 1]? | return true
      if inRange (prev, next) then
        xs := xs.eraseIdx i
      else if i >= 2 then
        let pprev := xs[i - 2]!
        if inRange (pprev, curr) then
          xs := xs.eraseIdx (i - 1)
        else
          return false
      else
        xs := xs.eraseIdx (i - 1)

    return true

#assert part2 with "Day2-example" is some 4

#run part2 with "Day2"

end AdventOfCode
