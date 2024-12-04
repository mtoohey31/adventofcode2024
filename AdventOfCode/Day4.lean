import AdventOfCode.Basic
import AssignOps

def Array.transpose [Inhabited α] (grid : Array (Array α)) := Id.run do
  let mut transposed := #[]
  for x in [:grid[0]!.size] do
    let mut line := #[]
    for y in [:grid.size] do
      line := line.push grid[y]![x]!
    transposed := transposed.push line
  return transposed

namespace AdventOfCode

inductive Letter where
  | X
  | M
  | A
  | S
deriving BEq, Inhabited, Ord

def Letter.ofChar? : Char → Option Letter
  | 'X' => X
  | 'M' => M
  | 'A' => A
  | 'S' => S
  | _ => none

def Letter.first := X

def Letter.next? : Letter → Option Letter
  | X => M
  | M => A
  | A => S
  | S => none

open Letter

instance : ToString Letter where
  toString
    | .X => "X"
    | .M => "M"
    | .A => "A"
    | .S => "S"

def parse (input : String) :=
  input.trim.splitOn "\n" |>.toArray.mapM (·.data.toArray.mapM Letter.ofChar?)

def ltr (grid : Array (Array Letter)) := Id.run do
  let mut next := Letter.X
  let mut count := 0
  for line in grid do
    next := .X
    for l in line do
      if l == next then
        if let some next' := next.next? then
          next := next'
        else
          count += 1
          next := first
      else if l == first then
        next := first.next?.get!
      else
        next := first
  return count

def tltobr (grid : Array (Array Letter)) := Id.run do
  let mut next := Letter.X
  let mut count := 0
  let height := grid.size
  let width := grid[0]!.size
  for xstart in [:width] do
    next := .X
    for x in [xstart:width], y in [:width - xstart] do
      let l := grid[y]![x]!
      if l == next then
        if let some next' := next.next? then
          next := next'
        else
          count += 1
          next := first
      else if l == first then
        next := first.next?.get!
      else
        next := first
  for ystart in [1:height] do
    next := .X
    for y in [ystart:height], x in [:height - ystart] do
      let l := grid[y]![x]!
      if l == next then
        if let some next' := next.next? then
          next := next'
        else
          count += 1
          next := first
      else if l == first then
        next := first.next?.get!
      else
        next := first
  return count

def part1 (input : String) := do
  let grid ← parse input
  let ltrGrids := #[grid, grid.map Array.reverse, grid.transpose, grid.transpose.map Array.reverse]
  let ltrSum := ltrGrids.map ltr |>.foldl Add.add 0
  let tltobrGrids := #[grid, grid.map Array.reverse, grid.reverse, grid.reverse.map Array.reverse]
  let tltobrSum := tltobrGrids.map tltobr |>.foldl Add.add 0
  return ltrSum + tltobrSum

#assert part1 with "Day4-example" is some 18

#run part1 with "Day4"

def part2 (input : String) := do
  let grid ← parse input
  let mut count := 0
  for x in [:grid[0]!.size - 2] do
    for y in [:grid.size - 2] do
      if grid[y + 1]![x + 1]! != A then
        continue
      let corners := #[grid[y]![x]!, grid[y + 2]![x]!, grid[y]![x + 2]!, grid[y + 2]![x + 2]!]
      if corners.qsortOrd == #[M, M, S, S] && grid[y + 2]![x]! != grid[y]![x + 2]! then
        count += 1
  return count

#assert part2 with "Day4-example" is some 9

#run part2 with "Day4"

end AdventOfCode
