import Lake
open Lake DSL

-- require Socket from git "https://github.com/alex-wellbelove/Socket.lean.git"@"main"
require Socket from ".."/"Socket.lean"
require LeanSqlite from ".."/"lean_sqlite"
require proofwidgets from git "https://github.com/EdAyers/ProofWidgets4"@"v0.0.15"

package «lean_server» {
}

lean_lib «LeanServer» {
  -- add library configuration options here
  srcDir := "LeanServer/lib"
}

@[default_target]
lean_exe «lean_server» {
  root := `Main
  moreLinkArgs := #["-L/opt/homebrew/opt/sqlite/lib","-lsqlite3"]
}
