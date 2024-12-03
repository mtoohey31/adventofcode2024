import AdventOfCode.Basic
import Regex

namespace AdventOfCode

open Regex

def evalMul (c : Captures) : Option Nat := do
  let xs ← (← c.groups[0]?)
  let ys ← (← c.groups[1]?)
  let x ← xs.toString.toNat?
  let y ← ys.toString.toNat?
  return x * y

def part1 (input : String) :=
  let re := regex% r"mul\((\d+),(\d+)\)"
  Regex.all_captures input re |>.mapM evalMul <&> Array.foldl Add.add 0

#assert part1 with "Day3-example" is some 161

#run part1 with "Day3"

def part2 (input : String) := Id.run do
  let re := regex% r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"
  let mut sum := 0
  let mut enabled := true
  for capture in Regex.all_captures input re do
    if enabled then
      if let some n := evalMul capture then
        sum := sum + n
      else if capture.fullMatch.bsize == 7 then
        enabled := false
    else if capture.fullMatch.bsize == 4 then
      enabled := true
  return sum

#assert part2 with "Day3-example" is 48

#run part2 with "Day3"

end AdventOfCode
