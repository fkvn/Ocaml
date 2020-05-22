let iffy (b:bool) : int =
  match b with
    | true -> 1
    | false -> 0
;;



iffy true;;
iffy false;;
