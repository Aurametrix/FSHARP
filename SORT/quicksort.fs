let identifier = lazy ( expression )

let rec quickSort = function
  | [] -> []
  | n::ns -> let lessthan, greaterEqual = List.partition ((>) n) ns
  quickSort lessthan @ n :: quickSort greaterEqual
