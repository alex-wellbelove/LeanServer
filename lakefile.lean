import Lake
open Lake DSL

require Socket from git "https://github.com/alex-wellbelove/Socket.lean.git"@"main"
require LeanSqlite from git "https://github.com/alex-wellbelove/lean_sqlite.git"@"main"
require proofwidgets from git "https://github.com/EdAyers/ProofWidgets4"@"v0.0.15"

package «lean_server» {
}

@[default_target]
lean_lib «LeanServer» {
  srcDir := "LeanServer/lib"
}
