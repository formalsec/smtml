let pp_exit_status fmt = function
  | Unix.WEXITED n -> Fmt.pf fmt "Exited %d" n
  | WSIGNALED n -> Fmt.pf fmt "Signaled %d" n
  | WSTOPPED n -> Fmt.pf fmt "Stopped %d" n
