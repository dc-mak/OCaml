module type DEQUEUE = sig
    type 'a queue

    val empty : 'a queue
    val isEmpty : 'a queue -> bool

    (* insert, inspect and remove the front element *)
    val cons     : 'a -> 'a queue -> 'a queue
    val head_exn : 'a queue -> 'a
    val tail_exn : 'a queue -> 'a queue

    (* insert, inspect and remove the rear element *)
    val snoc     : 'a -> 'a queue -> 'a queue
    val last_exn : 'a queue -> 'a
    val init_exn : 'a queue -> 'a queue

end

module Dequeue  : DEQUEUE  = struct
    type 'a queue =  'a list * 'a list

    let empty = ([], []) 

    open Core.Std

    let isEmpty = function
        | [], [] ->
            true

        | _, _ ->
            false

    (* First case:
     * Change in potential = -1 and cost of op is 1 so net is 0.
     *
     * Second case:
     * Change in potential = +1 and cost of op is 1 so net is 2. *)
    let cons x = function
        | [y], []
        | [], [y] ->
            ([x], [y])

        | f, r ->
            (x :: f, r)

    (* Data structure not modified, cost of op is 1 (by invariants). *)
    let head_exn = function
        | [], [] ->
            failwith "Empty"

        | [], _ :: _ :: _ -> 
            failwith "Invariant"

        | [], [x]
        | x :: _, _ ->
            x

    (* Case 1 element:
     * Change in potential = -1 and cost is 1 so net is 0.
     *
     * Case |f| = 1:
     * Change in potential = (1 or 0) - |1 - |r|| = (1 or 2) - r
     * (depending on r being odd or even). 
     * Actual cost (length, split, reverse) is O(r) (so without loss of
     * generality, assume r) net is 1 or 2.
     * (Could also change potential to be k * |f-r|)
     *
     * Case |f| >= 2:
     * Change in potential -1 and cost is 1 so net is 0. *)
    let tail_exn = function
        (* 0, 0 *)
        | [], [] ->
            failwith "Empty"

        (* 0, 1 *)
        | [], [_] ->
            ([], [])

        (* 0, 2+ *)
        | [], _ :: _ :: _
        (* 2+, 0 *)
        | _ :: _ :: _, [] ->
            failwith "Invariant"

        (* 1, 0+ *)
        | [_], r ->
            let r, f_rev = List.split_n r (List.length r / 2) in
            (List.rev f_rev, r)

        (* 2+, 1+ *)
        | _ :: (_ :: _ as f), (_ :: _  as r) ->
            (f, r)

    (* Same as cons. *)
    let snoc x = function
        | [y], []
        | [], [y] ->
            ([y], [x])
            
        | f, r ->
            (f, x :: r)

    (* Same as last_exn. *)
    let last_exn = function
        | [], [] ->
            failwith "Empty"

        | _ :: _ :: _, [] ->
            failwith "Invariant"

        | [x], []
        | _, x :: _ ->
            x

    (* Same as tail_exn. *)
    let init_exn = function
        (* 0, 0 *)
        | [], [] ->
            failwith "Empty"

        (* 1, 0 *)
        | [_], [] ->
            ([], [])

        (* 2+, 0 *)
        | _ :: _ :: _, []
        (* 0, 2+ *)
        | [], _ :: _ :: _ ->
            failwith "Invariant"

        (* 0+, 1 *)
        | f, [_] ->
            let f, r_rev = List.split_n f (List.length f / 2) in
            (f, List.rev r_rev)

        (* 1+, 2+ *)
        | _ :: _ as f, _ :: ( _ :: _ as r) ->
            (f, r)

end

(* Easier to understand version:

let norm : 'a queue -> 'a queue = function
    | [], [] as q ->
        q 

    | [], (_ :: _ as r) ->
        let r, f_rev = List.split_n r (List.length r / 2) in
        (List.rev f_rev, r)

    | _ :: _ as f, [] ->
        let f, r_rev = List.split_n f (List.length f / 2) in
        (f, List.rev r_rev)

    | _ :: _ , _ :: _ as q ->
        q
let cons x (f, r) = norm (x :: f, r)

let head_exn = function
    | x :: _, _ ->
        x

    | [], r ->
        List.last_exn r

let tail_exn = function
    | _ :: f, r ->
        norm (f, r)

    | [], r ->
        norm ([], List.tl_exn r)

let snoc x (f, r) = norm (f, x :: r)

let last_exn = function
    | f, [] ->
        List.last_exn f

    | _, x :: _ ->
        x

let init_exn = function
    | f, _ :: r ->
        norm (f, r)

    | f, [] ->
        let rec dropLast = function
            | [] ->
                failwith "Empty"
            
            | x :: xs ->
                x :: dropLast xs in
        norm (dropLast f, [])

*)
