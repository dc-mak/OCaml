(*
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
____________________________________________________________________________________________________
_________________________1_______________1_______________1_______________1__________________________
__________________________1_____________1_________________1_____________1___________________________
___________________________1___________1___________________1___________1____________________________
____________________________1_________1_____________________1_________1_____________________________
_____________________________1_______1_______________________1_______1______________________________
______________________________1_____1_________________________1_____1_______________________________
_______________________________1___1___________________________1___1________________________________
________________________________1_1_____________________________1_1_________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
_________________________________1_______________________________1__________________________________
__________________________________1_____________________________1___________________________________
___________________________________1___________________________1____________________________________
____________________________________1_________________________1_____________________________________
_____________________________________1_______________________1______________________________________
______________________________________1_____________________1_______________________________________
_______________________________________1___________________1________________________________________
________________________________________1_________________1_________________________________________
_________________________________________1_______________1__________________________________________
__________________________________________1_____________1___________________________________________
___________________________________________1___________1____________________________________________
____________________________________________1_________1_____________________________________________
_____________________________________________1_______1______________________________________________
______________________________________________1_____1_______________________________________________
_______________________________________________1___1________________________________________________
________________________________________________1_1_________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
_________________________________________________1__________________________________________________
*)

(* Enter your code here. Read input from STDIN. Print output to STDOUT *)

let drawY arr ~mid ~base half_h =
    for i = base downto base - half_h + 1 do
        arr.(i).(mid) <- '1'
    done;

    for i = 1 to half_h do
        arr.(base - half_h + 1 - i).(mid - i) <- '1';
        arr.(base - half_h + 1 - i).(mid + i) <- '1'
    done

let rec draw arr ~mid ~base ~h = function
    | 0 ->
        ()

    | n -> 
        let half_h, base' = h / 2, base - h in 
        drawY arr ~mid:mid ~base:base half_h;
        draw  arr ~mid:(mid - half_h) ~base:base' ~h:half_h (n-1);
        draw  arr ~mid:(mid + half_h) ~base:base' ~h:half_h (n-1)

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
    let _N = 6 in
    let w, h = 100, 2 *** _N in
    let arr = Array.make_matrix (h-1) w '_' in
    draw arr ~mid:(w / 2 - 1) ~base:(h - 2) ~h:(h / 2) (read_int());
    print_array print_char arr
