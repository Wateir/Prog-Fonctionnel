let musee = [
  "Astérix", "bouclier";
  "Astérix", "casque";
  "Astérix", "bouclier";
  "Astérix", "casque";
  "Astérix", "casque";
  "Astérix", "casque";
  "Obélix", "bouclier";
  "Obélix", "casque"
]

let rec insertObj element listecouple = match listecouple with
  |[] -> (1, element)::listecouple
  |(a,el)::listebis -> if el  == element 
      then
        (a+1, element)::listebis
      else
        insertObj element listecouple;;

let rec  insert index element listeindex = match listeindex with
  | [] -> (index, [1, element])::listeindex
  |(idx, listecouple)::resteliste -> if idx == index
      then
        (index, (insertObj element listecouple))::resteliste
      else
        insert index element resteliste;;

let rec decompte listeidxobjc = match listeidxobjc with
  | [] -> []
  | (a, b)::resteliste -> [insert a, b (decompte resteliste)];;

let rec longueur liste = match liste with
  |[] -> 0
  |(a, b)::resteliste -> a + longueur resteliste ;;

let rec aumoins_indexed n liste = match liste with
  |[] -> []
  |(a, listebis)::resteliste -> if (longueur listebis) >= n 
      then
        a::(aumoins_indexed n resteliste)
      else
        aumoins_indexed n resteliste;;

let aumoins n liste = aumoins_indexed n (decompte liste)

