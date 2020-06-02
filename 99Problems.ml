(* Find length of list. Replicate List.length *)
let find_length lst = 
    let rec aux length _lst = match _lst with 
        | [] -> length 
        | _::tl -> aux (length + 1) tl 
    in aux 0 lst;;

(* Find kth element in list. Replicate List.nth *)
let find_kth k lst = 
    let rec aux counter = function
        | [] -> None
        | hd::tl -> if counter = k then Some hd else aux (counter + 1) tl
    in aux 0 lst;;