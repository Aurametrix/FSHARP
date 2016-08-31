type 'a queue = NL| Que of 'a * 'a queue;;

let rec enque m = function
  NL          -> Que (m, NL)
| Que (x, xs) -> Que (x, enque m xs) // Note : not tail-rec

let rec peek  = function
    |NL -> failwith "queue is empty"
    |Que(x, xs) -> x;;

let rec deque  = function
    |NL -> failwith "queue is empty"
    |Que(x, xs) -> xs;;

let rec build = function
    | [] -> NL
    | x::xs -> enque x (build xs);;

