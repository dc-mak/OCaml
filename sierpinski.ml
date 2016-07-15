(* Enter your code here. Read input from STDIN. Print output to STDOUT *)

let rec fill arr ~l ~r ~base = function
    | 0 ->
        ()

    | h -> 
        for i = l to r do
            arr.(base).(i) <- '1'
        done;
        fill arr ~l:(l+1) ~r:(r-1) ~base:(base-1) (h-1)

(* Let i be the input value, N be the limit (N=5). Following are invariants
 * w = r - l + 1
 * w = 2 ^ (n + N - i + 1) - 1
 *
 * h = (r - l + 2) / 2 = w' + 1
 * h = 2 ^ (n + N - i)  *)
let rec draw arr ~l ~r ~w ~base ~h = function
    | 0 ->
        fill arr ~l:l ~r:r ~base:base h

    | n -> 
        let w', h' = w / 2, h / 2 in
        let w'' = w' / 2 in
        (* lower left *)
        draw arr ~l:l ~r:(l+w'-1) ~w:w' ~base:base ~h:h' (n-1);
        (* lower right *)
        draw arr ~l:(r-w'+1) ~r:r ~w:w' ~base:base ~h:h' (n-1);
        (* top *)
        draw arr ~l:(l+w''+1) ~r:(r-w''-1) ~w:(w'-1) ~base:(base - h') ~h:h' (n-1)

let rec ( *** ) x = function
    | 0 ->
        1

    | n when n mod 2 = 0 ->
        ( x * x) *** (n / 2)

    | n (* when n mod 2 = 1 *) ->
        x * (x * x) *** (n / 2)

let print_array print_elem arr = 
    Array.iter (fun row -> Array.iter print_elem row; print_newline()) arr

let () =  
    let _N = 5 in
    let w, h = 2 *** (_N+1) - 1, 2 *** _N in
    let arr = Array.make_matrix h w '_' in
    draw arr ~l:0 ~r:62 ~w:w ~base:(h-1) ~h:h (read_int());
    print_array print_char arr
