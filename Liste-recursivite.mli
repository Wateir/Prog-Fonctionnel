let rec somme l = match l with
  | a::l' -> if l' = [] then a else a + (somme l')
  | _ -> failwith "bin non";;

BEST VERSION 

let rec somme l = match l with
  | [] -> 0
  | a::l' -> a + somme l';;


let rec longueur l = match l with
  | [] -> 0
  | a::l' -> 1 + longueur l';;

let dernier l = let rec aux l = match l with
    | a::[]-> a
    | a::l' -> aux l'
  in if l = [] then failwith "dernier : liste vide"
  else
    aux l;;

let rec append l1 l2 = match l1 with 
  | [] -> l2
  |l3::l4 -> l3::(append l4 l2);;


let rec reverse l = match l with
  |[] ->[] 
  |a::l' -> reverse l'@[a];;

let reversebis l = let rec aux bis l = match l with
    | [] -> res
    | x::l' -> aux x::res l'
  in aux [] l;;

let rec nombreoccurence element liste = match liste with
  | [] -> 0
  | a::listebis -> if a = element then 1 + nombreoccurence listebis 
      else nombreoccurence listebis;;
 
