(* 2.1: Suffixes *)
let rec suffixes = function 
    | [] ->
        [[]]

    | x::xs as ls ->
        ls :: suffixes xs
    
(* 2.2. Fewer comparisons for member *)
type 'a tree = Lf | Br of 'a tree * 'a * 'a tree

let rec member' x x' = function
    | Lf ->
        x = x'

    | Br(l, v, r) when x < v ->
        member' x x' l

    | Br(l, v, r) ->
        member' x v r

let rec member x = function
    | Lf ->
        false

    | Br(l, v, r) when x < v ->
        member x l

    | Br(l, v, r) ->
        member' x v r

(* 2.3: Insertion with exceptions - prevent copies *)
exception Value_present

let rec ins' x = function
    | Lf ->
        Br (Lf, x, Lf)

    | Br (l, v, r) when x < v ->
        Br (ins' x l, v, r)

    | Br (l, v, r) when x > v ->
        Br (l, v, ins' x r)

    | Br _ ->
        raise Value_present

let rec ins x t =
    try ins' x t with Value_present -> t

(* 2.4: Combine the two ideas *)
let rec insert' x x' = function
    | Lf when x = x' ->
        raise Value_present

    | Lf ->
        Br (Lf, x, Lf)

    | Br (l, v, r) when x < v ->
        Br (insert' x x' l, v, r)

    | Br (l, v, r) ->
        Br (l, v, insert' x v  r)

let rec insert x = function
    | Lf ->
        Br (Lf, x, Lf)

    | Br (_, v, _) as t ->
        try insert' x v t with Value_present -> t

(* Some quick tests *)
let ins_test = List.fold_left (fun t x -> ins x t) Lf [4; 2; 5; 1; 3; 6; 7]
let insert_test = List.fold_left (fun t x -> insert x t) Lf [4; 2; 5; 1; 3; 6; 7];;
assert (ins_test = insert_test)
let member_test = List.map (fun x -> (x, member x insert_test)) [0; 1; 2; 3; 4; 5; 6; 7; 8];;
assert (
    member_test = [
        (0, false);
        (1, true);
        (2, true);
        (3, true);
        (4, true);
        (5, true);
        (6, true);
        (7, true);
        (8, false);
])

(* Ex 2.5: Sharing subtrees *)
let rec complete x = function
    | 0 -> 
        Lf

    | d ->
        let t = complete x (d-1) in
        Br (t, x, t)

(* Could add in a mutually recursive function to alternate position of k/k+1
 * subtrees per level/call *)
let odd n = n mod 2 = 1

let rec shared x = function
    | 0 -> 
        Lf

    | m when odd m ->
        let t = shared x (m / 2) in
        Br (t, x, t)

    | m (* even m *) ->
        let k = (m-1) / 2 in
        Br (shared x k, x, shared x (k + 1))

let rec size = function
    | Lf ->
        0

    | Br (l, x, r) ->
        1 + size l + size r;;

(* I WANT QUICKCHECK!!! *)
assert (size (complete 0 0) = 0);;
assert (size (complete 0 1) = 1);;
assert (size (complete 0 2) = 3);;
assert (size (complete 0 4) = 15);;

assert (size (shared 0 0) = 0);;
assert (size (shared 0 1) = 1);;
assert (size (shared 0 2) = 2);;
assert (size (shared 0 59) = 59);;

(* 2.6: Same as trees but use pairs: keys for comparisons, insert for bind,
 * projection for lookup. *)
