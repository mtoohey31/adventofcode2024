import AdventOfCode.Basic
import AssignOps

def Array.collate [BEq α] [Ord α] [Inhabited α] (xs : Array α) : Array (α × Nat) := Id.run do
  let mut prev : Option α := none
  let mut res := #[]
  for x in xs.qsortOrd do
    if .some x == prev then
      let (_, n) := res.back?.get!
      res := res.set! (res.size - 1) (x, n + 1)
    else
      res := res.push (x, 1)
    prev := .some x
  res

namespace AdventOfCode

def parse (input : String) := input.splitOn "\n" |>.toArray.pop.mapM toPair
where
  toPair (line : String) : Option (Int × Int) := do
    let [x, y] := line.splitOn "   " | none
    let x ← x.toInt?
    let y ← y.toInt?
    return (x, y)

def part1 (input : String) := do
  let pairs ← parse input
  let (lefts, rights) := pairs.unzip
  let sortedPairs := lefts.qsortOrd.zip rights.qsortOrd
  return Nat.sum <| Array.toList <| sortedPairs.map fun (x, y) => Int.natAbs (x - y)

#assert part1 with "Day1-example" is some 11

#run part1 with "Day1"

def part2 (input : String) := do
  let pairs ← parse input
  let (lefts, rights) := pairs.unzip
  let mut collatedLeft := lefts.collate
  let mut collatedRight := rights.collate
  let mut score : Int := 0
  repeat
    let some (left, leftCount) := collatedLeft.back? | return score
    let some (right, rightCount) := collatedRight.back? | return score
    if left <= right then
      if left == right then
        score += left * leftCount * rightCount
        collatedLeft := collatedLeft.pop
      collatedRight := collatedRight.pop
    else
      collatedLeft := collatedLeft.pop
  return score

#assert part2 with "Day1-example" is some 31

#run part2 with "Day1"

end AdventOfCode
