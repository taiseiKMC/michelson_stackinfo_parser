open Miniml.Cui

let () = 
  let inchannel= if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin in
  read_eval_print inchannel
