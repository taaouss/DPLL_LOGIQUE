(* MP1 2023/2024 - dpll.ml *)

open List

(* fonctions utilitaires *)
(* ----------------------------------------------------------- *)
(* filter_map : ('a -> 'b option) -> 'a list -> 'b list
   disponible depuis la version 4.08.0 de OCaml dans le module List :
   pour chaque élément de `list', appliquer `filter' :
   - si le résultat est `Some e', ajouter `e' au résultat ;
   - si le résultat est `None', ne rien ajouter au résultat.
   Attention, cette implémentation inverse l'ordre de la liste *)
let filter_map filter list =
  let rec aux list ret =
    match list with
    | [] -> ret
    | h :: t ->
      (match filter h with
       | None -> aux t ret
       | Some e -> aux t (e :: ret))
  in
  aux list []
;;

(* print_modele : int list option -> unit
   affichage du résultat *)
let print_modele : int list option -> unit = function
  | None -> print_string "UNSAT\n"
  | Some modele ->
    print_string "SAT\n";
    let modele2 = sort (fun i j -> abs i - abs j) modele in
    List.iter
      (fun i ->
        print_int i;
        print_string " ")
      modele2;
    print_string "0\n"
;;

(* ensembles de clauses de test *)
let exemple_3_12 = [[1;2;-3];[2;3];[-1;-2;3];[-1;-3];[1;-2]]
let exemple_7_2 = [[1;-1;-3];[-2;3];[-2]]
let exemple_7_4 = [[1;2;3];[-1;2;3];[3];[1;-2;-3];[-1;-2;-3];[-3]]
let exemple_7_8 = [[1;-2;3];[1;-3];[2;3];[1;-2]]
let systeme = [[-1;2];[1;-2];[1;-3];[1;2;3];[-1;-2]]
let coloriage = [
  [1;2;3];[4;5;6];[7;8;9];[10;11;12];[13;14;15];[16;17;18];
  [19;20;21];[-1;-2];[-1;-3];[-2;-3];[-4;-5];[-4;-6];[-5;-6];
  [-7;-8];[-7;-9];[-8;-9];[-10;-11];[-10;-12];[-11;-12];[-13;-14];
  [-13;-15];[-14;-15];[-16;-17];[-16;-18];[-17;-18];[-19;-20];
  [-19;-21];[-20;-21];[-1;-4];[-2;-5];[-3;-6];[-1;-7];[-2;-8];
  [-3;-9];[-4;-7];[-5;-8];[-6;-9];[-4;-10];[-5;-11];[-6;-12];
  [-7;-10];[-8;-11];[-9;-12];[-7;-13];[-8;-14];[-9;-15];[-7;-16];
  [-8;-17];[-9;-18];[-10;-13];[-11;-14];[-12;-15];[-13;-16];
  [-14;-17];[-15;-18]]

(* ----------------------------------------------------------- *)

(* simplifie : int -> int list list -> option list list 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)

let simplifie l clauses =
  (* Définition de la fonction auxiliaire `aux_filtre` qui simplifie les clauses *)
  let aux_filtre element liste =
    match List.mem element liste with
    | true -> None 
    (* Si le littéral `element` est dans la clause, la clause est éliminée  *)
    | false ->
      (* Sinon le littéral `element` n'est pas dans la clause*)
      (match List.mem (-element) liste with
       (* Si le complémentaire de `element` est dans la clause, on supprime le complémentaire *)
       | true -> Some (List.filter (fun x -> x <> -element) liste)
       (* Si ni `element` et ni son complémentaire ne sont dans la clause, on laisse la clause inchangée *)
       | false -> Some liste)
    (* On applique la fonction `aux_filtre` à chaque clause dans `clauses` en appellant `filter_map` *)
  in
  filter_map (aux_filtre l) clauses
;;

(* solveur_split : int list list -> int list -> int list option
   exemple d'utilisation de `simplifie' *)
(* cette fonction ne doit pas être modifiée, sauf si vous changez 
   le type de la fonction simplifie *)

let rec solveur_split clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = []
  then Some interpretation
  else if (* la clause vide n'est jamais satisfiable *)
          mem [] clauses
  then None
  else (
    (* branchement *)
    let l = hd (hd clauses) in
    let branche = solveur_split (simplifie l clauses) (l :: interpretation) in
    match branche with
    | None -> solveur_split (simplifie (-l) clauses) (-l :: interpretation)
    | _ -> branche)
;;

(* tests *)
(* let () = print_modele (solveur_split systeme []) *)
(* let () = print_modele (solveur_split coloriage []) *)

(* solveur dpll récursif *)
(* ----------------------------------------------------------- *)

(* pur : int list list -> int
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon, lève une exception `Failure "pas de littéral pur"' *)

let pur clauses =
  let liste_concatinee = List.flatten clauses in
  (*Concatener toutes les clause de l'ensemble en une seule clause*)
  (* Définition de la fonction recursive auxiliaire `pur_aux` qui verifie s'il existe un littéral pur dans `list` *)
  let rec pur_aux list =
    match list with
    (* pas de clause trouvée , l'ensemble est vide on lève une exception `Failure "pas de littéral pur" *)
    | [] -> raise (Failure "pas de littéral pur")
    | litteral :: reste_liste ->
      (*On cherche le complementaire du litteral dans l'ensemble de clauses *)
      (match List.mem (-litteral) reste_liste with
       (*Complementaire du litteral trouvé donc il n'est pas pur, on supprime toutes ses occurences et 
          celles de son complementaire de l'ensemble de clauses et on continue la recherche litteral pur *)
       | true -> pur_aux (List.filter (fun x -> x <> -litteral && x <> litteral) reste_liste)
       (*le complementaire n'existe pas dans l'ensemble de clause donc le litteral est pur et on le retourne*)
       | false -> litteral)
  in
  pur_aux liste_concatinee
;;

(* unitaire : int list list -> int
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon, lève une exception `Not_found' *)

let rec unitaire clauses =
  match clauses with
  | [] -> raise Not_found
    (* pas de clause trouvée , l'ensemble est vide donc on lève une exception `Not_found'*)
  | clause :: reste_clauses ->
    (match List.length clause with
     (* si la clause a un seul litteral  donc on retourne ce littéral  *)
     | 1 -> List.hd clause
     (* sinon, pas de clause unitaire dans cette clause, on continue la recherche dans le reste des clauses *)
     | _ -> unitaire reste_clauses)
;;


(* solveur_dpll_rec : int list list -> int list -> int list option *)
let rec solveur_dpll_rec clauses interpretation =
   
  match clauses with
  | [] -> Some interpretation
  (* si l'ensemble est vide alors il est SAT *)
  | _ when List.mem [] clauses -> None
  (* si l'ensemble contient la clause vide alors il est NON SAT *)
  | _ ->
    (try
      (*On commence par chercher s'il existe une clause unitaire*)
       match unitaire clauses with
       (*Clause unitaire trouvée, on simplifie l'ensemble et on ajoute la valeur de vérité de la clause unitaire à l'interprétation*)
       |  u -> solveur_dpll_rec (simplifie u clauses) (u :: interpretation)
     with
     (*Pas de clause unitaire*)
     | Not_found ->
       (try
          (*On continue par chercher s'il existe une clause avec un littéral pur*)
          match pur clauses with
          (*Littéral pur trouvé, on simplifie l'ensemble et on ajoute la valeur de vérité du littéral à l'interprétation*)
          |  p -> solveur_dpll_rec (simplifie p clauses) (p :: interpretation)
        with
        (*Pas de littéral pur alors on fait appel à `solveur_split` *)
        | Failure "pas de littéral pur" -> solveur_split clauses interpretation))
;;

(* tests *)
(* ----------------------------------------------------------- *)
(* let () = print_modele (solveur_dpll_rec systeme []) *)
(* let () = print_modele (solveur_dpll_rec coloriage []) *)
let () =
  let resultat_split = solveur_dpll_rec coloriage [] in
  print_modele resultat_split
;;
