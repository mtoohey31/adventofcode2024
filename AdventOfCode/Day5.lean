import AdventOfCode.Basic
import AssignOps

namespace AdventOfCode

open Std

abbrev Rules := HashMap Nat <| HashSet Nat
abbrev Update := Array Nat

def parse (input : String) : Option (Rules × Array Update) := do
  let [rules, updates] := input.trim.splitOn "\n\n" | none
  let rules ← rules.splitOn "\n" |>.mapM toRule
  let updates ← updates.splitOn "\n" |>.toArray.mapM toUpdate
  return (
    rules.foldl (init := .empty)
      fun rs (before, after) => rs.insert after <| (rs.getD after default).insert before,
    updates
  )
where
  toRule (s : String) := do
    let [before, after] := s.splitOn "|" | none
    let before ← before.toNat?
    let after ← after.toNat?
    return (before, after)
  toUpdate (s : String) := s.splitOn "," |>.toArray.mapM String.toNat?

def Update.correctlyOrdered (u : Update) (rs : Rules) := Id.run do
  let mut disallowed := HashSet.empty
  for page in u do
    if disallowed.contains page then
      return false

    if let some before := rs[page]? then
      disallowed := disallowed.union before
  return true

def Update.middle (u : Update) := u[u.size / 2]!

open Update

def part1 (input : String) := do
  let (rules, updates) ← parse input
  return Nat.sum <| updates.filter (correctlyOrdered · rules) |>.map middle |>.toList

#assert part1 with "Day5-example" is some 143

#run part1 with "Day5"

def Update.reorder (u : Update) (rs : Rules) : Update := Id.run do
  let mut u' := #[]
  let mut remainingPages := HashSet.ofArray u
  while !remainingPages.isEmpty do
    for remainingPage in remainingPages do
      if rs.getD remainingPage .empty |>.any remainingPages.contains then
        continue

      remainingPages := remainingPages.erase remainingPage
      u' := u'.push remainingPage
  return u'

def part2 (input : String) := do
  let (rules, updates) ← parse input
  let incorrectlyOrdered := updates.filter (!correctlyOrdered · rules)
  return Nat.sum <| incorrectlyOrdered |>.map (reorder · rules) |>.map middle |>.toList

#assert part2 with "Day5-example" is some 123

#run part2 with "Day5"

end AdventOfCode
