(* Dhruv Makwana, Trinity College *)
(* Q4 in Example Sheet 4 for ML & BayInfer *)
(* Viterbi Algorithm *)

(* Transition matrix: Pr(S_t+1 | S_t) *)
let trans prev next = match prev, next with
  | 1,1 -> 0.2 | 1,2 -> 0.4 | 1,3 -> 0.4
  | 2,1 -> 0.1 | 2,2 -> 0.6 | 2,3 -> 0.3
  | 3,1 -> 0.8 | 3,2 -> 0.1 | 3,3 -> 0.1
  | _, _ -> failwith "transition undefined"
;;

(* Evidence *)
type shape = Circle | Up | Down | Square
;;

(* Evidence matrix: Pr(evidence|state) *)
let e state ev = match state, ev with
  | 1, Up -> 0.4 | 1, Down -> 0.4| 1, Circle -> 0.2 | 1, Square -> 0.1
  | 2, Up -> 0.6 | 2, Down -> 0.3| 2, Circle -> 0.1 | 2, Square -> 0.1
  | 3, Up -> 0.1 | 3, Down -> 0.1| 3, Circle -> 0.8 | 3, Square -> 0.2
  | _, _ -> failwith "P(e|s) undefined"
;;

(* Sequence of evidence given *)
let seq =
  [ Circle
  ; Circle
  ; Square
  ; Square
  ; Up
  ; Up
  ; Square
  ; Down
  ; Square
  ]
;;

(* Initial state probabilities *)
let init = function
  | 1 -> 0.3
  | 2 -> 0.4
  | 3 -> 0.3
  | _ -> failwith "initial undefined"
;;

(* Return state : int and result : int list and value : float which maximises f *)
let maximise (f : int -> (int list * float)) : (int * int list * float) = 
  let result1, value1 = f 1 in
  let result2, value2 = f 2 in
  let result3, value3 = f 3 in 
  if value1 > value2 && value1 > value3 then
    (1, result1, value1)
  else if value2 > value1 && value2 > value3 then
    (2, result2, value2)
  else
    (3, result3, value3)
;;

(* Recurse on sequence "in reverse" *)
let rec viterbi (curr : int) : shape list -> (int list * float) = function
  | [first] -> 
    let base start = ([], trans start curr *. init start) in
    let (start, path, value) = maximise base in
    (start :: path, e curr first *. value)

  | ev :: rest ->
    let recurse prev = 
      let path, value = viterbi prev rest in
      (path, trans prev curr *. value) in
    let prev, sofar, value = maximise recurse in
      (prev :: sofar, e curr ev *. value)

  | _ -> failwith "viterbi undefined"
;;

(* Let it run *)
let most_likely = 
  let find_likely final = viterbi final (List.rev seq) in
  let (last, rev_seq, value) = maximise find_likely in
  (List.rev (last :: rev_seq), value)
;;

(* val most_likely : int list * float =
     ([2; 1; 2; 1; 2; 1; 3; 2; 1; 2], 1.54618822656000127e-08)
 *)
