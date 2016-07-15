(* 3.6: Remove rank field from node *)
type 'a tree = Node of 'a * 'a tree list
type 'a heap = (int * 'a tree) list

let empty : 'a heap = []
let isEmpty : 'a heap -> bool = function
    | [] -> true
    | _::_ -> false

let rank (Node (x, children)) = List.length children 
let root (Node (x, children)) = x

let link (Node (x1, c1) as t1, (Node (x2, c2) as t2)) =
    if x1 <= x2 then
        Node (x1, t2 :: c1) 
    else
        Node (x2, t1 :: c2)

let rec insTree = function
    | t, [] -> [t]

    | (r,t) as t1, ((r',t') :: ts' as ts) ->
        if r < r' then
            t1 :: ts
        else 
            insTree ((r, link (t, t')), ts')

let insert (x, ts) = insTree ((0, Node (x, [])), ts)

let rec merge : 'a heap * 'a heap -> 'a heap = function
    | ts, []
    | [], ts -> ts

    | ((r1,t) as t1) :: t1s' as t1s, (((r2,t') as t2) :: t2s' as t2s) ->
        if r1 < r2 then
            t1 :: merge (t1s', t2s)
        else if r1 > r2 then
            t2 :: merge (t1s, t2s') 
        else
            insTree ((r1, link (t, t')), merge (t1s', t2s'))

let rec removeMinTree :  'a heap -> 'b tree *  'a heap  = function
    | [] ->
        failwith "Empty"

    | [(_,t)] ->
        (t, [])

    | (_, t) as t1 :: ts -> 
        let t', ts' = removeMinTree ts in
        if root t < root t' then
            (t, ts)
        else 
            (t', t1 :: ts')

let findMin ts = root (fst (removeMinTree ts))

(* 3.5: Define findMin directly rather than a call to removeMinTree *)
let rec findMin' = function
    | [] -> failwith "Empty"

    | [(_,t)] -> root t

    | (_, t) :: ts ->
        let m1, m2 = root t, findMin' ts in
        if m1 < m2 then m1 else m2

let deleteMin ts : 'a heap =
    let Node (x, t1s), t2s = removeMinTree ts in
    merge (List.rev (List.map (fun x -> (rank x, x)) t1s), t2s)

module type HEAP = sig
    type 'a heap 
    val empty : 'a heap
    val isEmpty : 'a heap -> bool
    val insert : 'a *  'a heap -> 'a heap
    val merge : 'a heap * 'a heap -> 'a heap
    val findMin : 'a heap -> 'a
    val deleteMin :  'a heap -> 'a heap
end

module ExplicitMin (H : HEAP) : HEAP = struct
    type 'a heap = E | NE of 'a * 'a H.heap

    let empty = E

    let isEmpty = function
        | E -> true
        | NE _ -> false

    let insert (x,h) = match h with
        | E ->
            NE (x, H.insert (x, H.empty))

        | NE (x', h)  ->
            NE ((if x < x' then x else x'), H.insert (x, h))

    let merge = function 
        | h, E 
        | E, h ->
            h

        | NE (x1, h1), NE (x2, h2) ->
            NE ((if x1 < x2 then x1 else x2), H.merge (h1, h2))

    let findMin = function
        | E -> failwith "Empty"

        | NE (x, h) -> x

    let deleteMin = function
        | E ->
            failwith "Empty"

        | NE (x, h) -> 
            NE (H.findMin h, H.deleteMin h)
end

(* Ex 3.8: Maximum depth of a RB tree = 2 log (n+1)
 *  2^(d_max / 2) - 1 >= n - by 2*(shortest path) >= longest path
 *  d_max / 2 >= log (n+1)
 *  d_max >= 2 log (n+1) *)

(* Ex 3.9: Construct a RB from an ordered list. *)
type color = R | B
type 'a rb_tree = E | T of color * 'a rb_tree * 'a * 'a rb_tree

let rec fromOrdList' color = function
    | [] -> E

    | [x] -> 
        T (color, E, x, E)

    | _ :: _ as xs ->
        let module List = Core.Std.List in
        let len = List.length xs in (* len >= 2 *)
        let left = List.take xs (len / 2)
        and right = List.drop xs (len / 2)
        and opp = match color with  R -> B | B -> R in
        T ( color,
            fromOrdList' opp left,
            List.hd_exn right,
            fromOrdList' opp (List.tl_exn right)
        )

let fromOrdList t = fromOrdList' B t

(* 3.10 (a) Splitting balance into left and right components. *)
let lbalance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))

    | c, a, x, b ->
        T (c, a, x, b)

let rbalance = function
    | B, a, x, T (R, T (R, b, y, c), z, d) 
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))

    | c, a, x, b ->
        T (c, a, x, b)

(* 3.10 (b) No idea what they want, found this online, seems kinda pointless. *)
let insert (x, s) = 
    let rec ins = function
        | E ->
            T (R, E, x, E)
        
        | T (R, a, y, b) as s ->
            if x < y then
                T (R, ins a, y, b)
            else if x > y then
                T (R, a, y, ins b)
            else 
                s

        | T (B, a, y, b) as s ->
            if x < y then
                lbalance (B, ins a, y, b)
            else if x > y then
                rbalance (B, a, y, ins b)
            else 
                s

    in match ins s with
    | T (_, a, y, b) ->
        T (B, a, y, b)

    | E ->
        failwith "Incorrect assertion"

