let sort l = 
  let rec sortUtil acc rev l =
    match l, rev with
    | [], true -> acc |> List.rev
    | [], false -> acc |> List.rev |> sortUtil [] true
    | x::y::tl, _ when x > y -> sortUtil (y::acc) false (x::tl)
    | hd::tl, _ -> sortUtil (hd::acc) rev tl
  sortUtil [] true l
