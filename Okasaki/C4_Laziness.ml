(* 4.1: Show both functions are equivalent. 
 * fun lazy f p = e ~ fun f x = $ case x of p => force e
 *
 * fun lazy drop (0, s) = s
 *        | drop (n, $Nil) = $Nil
 *        | drop (n, $Cons(x,s)) = drop (n-1, s)
 *
 * desugars to
 * 
 * fun drop x = $ case x of 
 *     | (0, s) => force s
 *     | (n, $Nil) => force ($Nil) 
 *     | (n, $Cons(x,s)) => force (drop (n-1, s))
 *
 * and then 
 *
 * ---
 *
 * fun lazy drop (n, s) = 
 *     let fun drop' (0, s) = s
 *           | drop' (n, $Nil) = $Nil
 *           | drop' (n, $Cons(x,s)) = drop' (n-1, s)
 *     in  drop' (n, s) end
 *
 * desugars to
 *
 * fun drop x = $ case x of 
 *     | (n, s) => force (
 *          let fun drop' (0, s) = s
 *                | drop' (n, $Nil) = $Nil
 *                | drop' (n, $Cons(x,s)) = drop' (n-1, s)
 *          in  drop' (n, s) end)
 *
 * so for n = 0, both evaluate to $ (force s), for s = $Nil, both are $ (force $Nil)
 * and assuming both for drop (n-1, s) evaluate to the same, both functions
 * evaluate drop (n, s) as $ (force (drop' (n-1, s))) = $ (force (drop (n-1, s))). *)

(*
let rec insert x = function
    | [] -> [x]

    | y :: ys as xs ->
        if x > y then 
            y :: insert x ys
        else 
            x :: xs

let rec insertionSort = function
    | [] -> []

    | x :: xs ->
        insert x (insertionSort xs)
*)

type 'a stream = Nil | Cons of 'a * 'a stream lazy_t

let rec extractMin = function
    | x, Nil, rest ->
        print_string "insert\n";
        (x, rest)

    | x, Cons (y, ys), rest  -> 
        print_string "insert\n";
        if y < x then
            extractMin (y, Lazy.force ys, Cons (x, lazy rest))
        else 
            extractMin (x, Lazy.force ys, Cons (y, lazy rest))

(* Really, this is selection sort. *)
let rec insertionSort = function
    | Nil ->
        print_string "sort\n";
        Nil

    | Cons (x, xs) ->
        print_string "sort\n";
        let min, rest = extractMin (x, Lazy.force xs, Nil) in
        Cons (min, lazy (insertionSort rest))

let rec fromList = function
    | [] -> 
        print_string "fromList\n";
        Nil

    | x :: xs ->
        print_string "fromList\n";
        Cons (x, lazy (fromList xs))

let rec take n = function
    | Nil -> []
    
    | Cons (x, xl) ->
        if n = 0 then
            []
        else 
            x :: take (n-1) (Lazy.force xl)

let test = fromList [10;9;8;7;6;5;4;3;2;1;0;]

let check = take 4 (insertionSort test)
