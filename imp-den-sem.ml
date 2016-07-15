type aexp =
    | Int of int
    | Loc of string
    | Plus of aexp * aexp

type bexp = 
    | T
    | F
    | Eq of aexp * aexp
    | Not of bexp

type comm = 
    | Skip
    | Assign of string * aexp
    | Seq of comm * comm
    | If of bexp * comm * comm

let rec _A aexp state = match aexp with
    | Int n ->
        n

    | Loc l ->
        state l

    | Plus (a1, a2) ->
        _A a1 state + _A a2 state

let rec _B bexp state = match bexp with
    | T ->
        true
    
    | F ->
        false

    | Not b ->
        not (_B b state)

    | Eq (a1, a2) ->
        _A a1 state = _A a2 state

let rec _C cexp state = match cexp with
    | Skip ->
        state

    | Assign  (l, a) ->
        fun x -> if l=x then _A a state else state x

    | Seq (c1, c2) ->
        _C c2 (_C c1 state)

    | If (b, c1, c2) ->
        if _B b state then
            _C c1 state
        else
            _C c2 state
