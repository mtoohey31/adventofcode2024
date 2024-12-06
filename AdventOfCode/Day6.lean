import AdventOfCode.Basic
import AssignOps

namespace AdventOfCode

open Std

inductive Direction where
  | up
  | right
  | down
  | left
deriving BEq, Hashable

def Direction.ofChar? : Char → Option Direction
  | '^' => up
  | '>' => right
  | 'v' => down
  | '<' => left
  | _ => none

def Direction.rotate : Direction → Direction
  | up => right
  | right => down
  | down => left
  | left => up

instance : ToString Direction where
  toString
    | .up => "^"
    | .right => ">"
    | .down => "v"
    | .left => "<"

structure Pos where
  y : Nat
  x : Nat

def Pos.move? (pos : Pos) (width height : Nat) : Direction → Option Pos
  | .up => if pos.y > 0 then some { pos with y := pos.y - 1 } else none
  | .right => if pos.x + 1 < width then some { pos with x := pos.x + 1 } else none
  | .down => if pos.y + 1 < height then some { pos with y := pos.y + 1 } else none
  | .left => if pos.x > 0 then some { pos with x := pos.x - 1 } else none

def parse (input : String) : Option (Direction × Pos × Array (Array Bool)) := do
  let cells := input.trim.splitOn "\n" |>.map String.toList
  let [(dir, pos)] := cells.enum.map
    (fun (y, row) => row.enum.filterMap fun (x, c) => return (← Direction.ofChar? c, { y, x })) |>.join 
    | none
  return (dir, pos, cells.toArray.map fun row => row.toArray.map (· == '#'))

def mkGrid (height width : Nat) (x : α) :=
  List.iota height |>.toArray.map fun _ => List.iota width |>.toArray.map fun _ => x

def part1 (input : String) := do
  let (d, pos, grid) ← parse input
  let height := grid.size
  let width := grid[0]!.size
  let mut visited := mkGrid height width false
  let mut d := d
  let mut pos := pos
  repeat
    visited := visited.set! pos.y <| visited[pos.y]!.set! pos.x true
    let some pos'@{ y, x } := pos.move? width height d | break
    if grid[y]![x]! then
      d := d.rotate
    else
      pos := pos'
  return visited.toList.map Array.toList |>.join.countP id

#assert part1 with "Day6-example" is some 41

#run part1 with "Day6"

def loops (d : Direction) (pos : Pos) (grid : Array (Array Bool)) : Bool := Id.run do
  let height := grid.size
  let width := grid[0]!.size
  let mut visited := mkGrid height width HashSet.empty
  let mut d := d
  let mut pos := pos
  repeat
    let rowy := visited[pos.y]!
    let setyx := rowy[pos.x]!
    if setyx.contains d then
      return true
    visited := visited.set! pos.y <| visited[pos.y]!.set! pos.x <| setyx.insert d
    let some pos'@{ y, x } := pos.move? width height d | break
    if grid[y]![x]! then
      d := d.rotate
    else
      pos := pos'
  return false

def part2 (input : String) := do
  let (d, pos, grid) ← parse input
  let height := grid.size
  let width := grid[0]!.size
  let mut d := d
  let mut pos := pos
  let mut count := 0
  let mut tried := mkGrid height width false
  repeat
    let some pos'@{ y, x } := pos.move? width height d | break
    if grid[y]![x]! then
      d := d.rotate
    else
      if !tried[y]![x]! then
        tried := tried.set! y <| tried[y]!.set! x true
        if loops d.rotate pos <| grid.set! y <| grid[y]!.set! x true then
          count += 1
      pos := pos'
  return count

#assert part2 with "Day6-example" is some 6

#run part2 with "Day6"

end AdventOfCode
