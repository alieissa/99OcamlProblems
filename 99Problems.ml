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

(* Reverse list. Replicate List.rev *)
let reverse_list lst = 
    let rec aux acc = function
        | [] -> acc
        | hd::tl -> aux (hd::acc) tl
    in aux [] lst;; 

(* Find if list is a palindrome *)
let is_palindrome lst = reverse_list lst = reverse_list lst;;

(* Flatten list. Type of nested list provided in exercise*)
type 'a node = | One of 'a | Many of 'a node list;;
let flatten lst = 
    let rec aux acc = function 
        | [] -> acc 
        | One hd::tl -> aux hd::acc tl (* Could have simply used concat operator @ here to avoid reversing list at end*)
        | Many hd::tl -> aux (aux acc hd) tl
    in reverse_lst (aux [] lst);;

(* Pack consecutive duplicates of list elements into sublists *)
let pack lst = 
  let rec aux curr_pack acc_pack = function
    | [] -> acc_pack
    | hd::tl -> match curr_pack with
      | [] -> aux [hd] acc_pack tl
      | chd::_ ->
          if(chd = hd)
          then aux (hd::curr_pack) acc_pack tl
          else aux [hd] (curr_pack::acc_pack) tl
  in aux [] [] lst;;

(* Encode a list. *)
let encode lst =
    let rec aux acc = function
        | [] -> acc
        | hd::tl -> aux ((List.length hd, List.hd hd)::acc) tl
    in List.rev (aux [] (pack lst));;

(* Encode a list. Solution proposed in OCaml website. Uses less memory *)
let encode_solution lst = 
    let rec aux counter acc = function
        | [] -> acc
        | [x] -> (counter + 1, x)::acc
        | a::(b::_ as t) -> 
            if a = b
            then aux (counter + 1) acc t
            else aux 0 ((counter + 1, a)::acc) t
    in List.rev (aux 0 [] lst);;

type 'a rle = | One of 'a | Many of int*'a;;
let modified_encode lst =
  let get_type count elem =
    if count = 1
    then One elem
    else Many (count, elem)
  in 
  let rec aux counter acc = function
    | [] -> acc
    | [x] -> (get_type counter x)::acc
    | a::(b::_ as t) ->
        if a = b
        then aux (counter + 1) acc t
        else aux 0 ((get_type (counter + 1) a)::acc) t
  in List.rev (aux 0 [] lst);;

(* Remove consecutive duplicates from list. *)
let compress lst = match lst with
    | [] | [_] as l -> l
    | lst' -> 
        let rec aux acc = function 
            | a::(b::_ as t) -> if a = b then aux acc t else aux (b::acc) t 
            | l -> acc
        in aux [List.hd lst'] lst'
;;
(* Duplicate elements of a list *)
let duplicate lst =
    let rec aux acc = function
        | [] -> acc
        | hd::tl -> aux (hd::hd::acc) tl
    in List.rev (aux [] lst);;