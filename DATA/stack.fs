let popMany n (stack : Stack<'a>) =
     let noopReturn = [], stack
     if stack.length = 0 then noopReturn
     else
         match n with
         | x when x <= 0 || stack.length < n -> noopReturn
         | x -> 
             let rec popManyTail n st acc =
                 match n with
                 | 0 -> acc   // exit recursion
                 | _ -> 
                     let hd, tl = List.head st, List.tail st
                     popManyTail (n - 1) tl (hd::fst acc, StackNode(tl)) //keep track of intermediate results
             popManyTail n stack.asList ([],empty) // call the actual worker function
