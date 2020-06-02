(* Find length of list. Replicate List.length *)
let find_length lst = 
    let rec aux length _lst = match _lst with 
        | [] -> length 
        | _::tl -> aux (length + 1) tl 
    in aux 0 lst;;