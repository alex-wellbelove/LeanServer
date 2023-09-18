import Lake
open Lake DSL

-- require Socket from git "https://github.com/alex-wellbelove/Socket.lean.git"@"main"
require Socket from ".."/"Socket.lean"

package «lean_server» {
}

lean_lib «LeanServer» {
  -- add library configuration options here
}
@[default_target]
lean_exe «lean_server» {
  root := `Main
}
