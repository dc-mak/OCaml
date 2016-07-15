(*
 * Heap: each node is less than or equal to its children.
 * Rank: length of right-spine (rightmost path from node to leaf).
 *       always shortest path to leaf.
 * Leftist-heap: for Br (l, x, r), rank l >= rank r.
 *)

(*
 * Ex 3.1: Prove rank (leftist n) <= log (n+1).
 *         Since rank is length of shortest path to leaf, we know
 *         n >= 2 ^ rank - 1 (number of nodes in a tree of height rank)
 *         so rank <= log (n + 1)
 *)

(* Boiler-plate *)
type 'a heap =
    | E
    | T of int * 'a * 'a heap *  'a heap

let rank = function
    | E -> 0 
    | T (r, _, _, _) -> r

let makeT = function 
    | x, l, r when rank l >= rank r ->
        T (rank r + 1, x, l, r)

    | x, l, r -> 
        T (rank l + 1, x, r, l)

let rec merge = function
    | h, E
    | E, h -> h

    | T(_, x, l1, r1) as h1, (T(_, y, l2, r2) as h2) ->
        if x < y then
            makeT (x, l1, merge (r1, h2))
        else
            makeT (y, l2, merge (h1, r2))

let insert x h = merge (T(1,x,E,E), h)

(* Ex 3.2: Insert defined directly instead of via merge. *)
let rec insert' x = function
    | E ->
        T (1, x, E, E)

    | T (_, v, _, r) as h when x < v ->
        T (1, x, h, E)

    | T (_, v, l, r) ->
            makeT (v, l, insert' x r);;

let heap1 = List.fold_left (fun h x -> insert x h) E [4; 2; 5; 1; 3; 6; 7]
let heap2 = List.fold_left (fun h x -> insert' x h) E [4; 2; 5; 1; 3; 6; 7];;
assert (heap1 = heap2)

(* Ex 3.3: Prove that fromList takes only O(n) time. *)
let fromList xs =
    let rec pair_merge = function
        | [] -> []
        | [h] -> [h]
        | x :: y :: xs -> merge (x,y) :: pair_merge xs in

    let rec extract = function
        | [] -> E
        | [h] -> h
        | _ :: _ :: _ as l -> extract (pair_merge l) in

    extract (List.map (fun x -> T(1,x,E,E)) xs)

let rec toList = function
    | E -> []
    | T (_, x, l, r) -> toList l @ [x] @ toList r

let list1 = [4; 2; 5; 1; 3; 6; 7]
let heap3 = fromList list1
let list2 = toList heap3;;
assert (List.sort (-) list1 = List.sort (-) list2)

(* Pass  | Heaps
 * ----- | ------
 * 0     | n
 * 1     | n/2
 * 2     | n/4
 * .     | .
 * log n | 1 = n/2^(log n)
 * so it's bounded by n * ( 1 + 1/2 + ... ) = 2n = O (n) [CLRS3 - Heaps] *)

(* Ex. 3.4: Weight-biased leftist heaps: size l >= size r. *)
(* a - Same argument as before: n >= 2^(d+1) - 1 *)
(* b - Modify makeT to take in so that's it's rank l + rank r (instead of ... + 1) *)
let rec makeT_weight (x, l, r) =
    let wl, wr = rank l, rank r in 
    if wl >= wr then 
        T (wl + wr, x, l, r)
    else
        T (wl + wr, x, r, l)

(* c - Single top-down pass for weight-biased [abuse of notation by using rank]
 *     Basic inlining and reducing, factored out makeT_merge because duplicated logic
 * d - Lazy: evaluates only when necessary
 *     Concurrent: less contention/time to create? *)
let rec merge_weight (hx, hy) =

    let makeT_merge (x, l, r1, r2) = 
        let wl, wr = rank l, rank r1 + rank r2 in
        if wl >= wr then
            T (wl + wr, x, l, merge_weight (r1, r2))
        else 
            T (wl + wr, x, l, merge_weight (r1, r2)) in

    match hx, hy with
    | h, E
    | E, h -> h

    | T(_, x1, l1, r1), (T(_, x2, _, _) as h2) when x1 < x2 ->
            makeT_merge (x1, l1, r1, h2)

    | T(_, x1, _, _) as h1, T(_, x2, l2, r2) (* when x1 >= x2 *) ->
            makeT_merge (x2, l2, h1, r2)
