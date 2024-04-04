let rec somme l = match l with
  | a::l' -> if l' = [] then a else a + (somme l')
  | _ -> failwith "bin non";; 

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
    | [] -> bis
    | x::l' -> aux (x::bis) l'
  in aux [] l;;

let rec nombreoccurence element liste = match liste with
  | [] -> 0
  | a::l' -> if a = element then 1 + nombreoccurence element l'
      else nombreoccurence element l';;

let rec prem elem l=match l with
  |[]->[]
  |a::l1->if elem = a then prem elem l1 
      else prem (a::l1) ;;

let dern l = reverse(prem(reverse l));;
 
