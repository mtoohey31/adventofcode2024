import Lean

namespace AdventOfCode

open IO.FS

private
def run [ToString α] (answerFunc : String → α) (inputName : String)
  : Lean.Elab.Term.TermElabM Unit := do
  let input ← readFile <| "input/" ++ inputName
  Lean.logInfo <| toString <| answerFunc input

macro "#run " answerFunc:term " with " inputName:str : command =>
  `(#eval run $answerFunc $inputName)

private
def assert [BEq α] [ToString α] (answerFunc : String → α) (inputName : String) (expected : α)
  : Lean.Elab.Term.TermElabM Unit := do
  let input ← readFile <| "input/" ++ inputName
  let actual := answerFunc input
  if actual == expected then
    Lean.logInfo "✓"
  else
    throwError s!"expected: {expected}, got: {actual}"

macro "#assert " answerFunc:term " with " inputName:str " is " expected:term : command =>
  `(#eval assert $answerFunc $inputName $expected)

end AdventOfCode
