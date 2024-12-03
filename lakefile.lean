import Lake
open Lake DSL

package AdventOfCode

require AssignOps from git "https://github.com/mtoohey31/lean4-assignops" @ "main"
require Regex from git "https://github.com/bergmannjg/regex" @ "main"

@[default_target]
lean_lib AdventOfCode
