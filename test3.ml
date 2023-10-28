let filter_map filter list =
  let rec aux list ret =
    match list with
    | []   -> ret
    | h::t -> match (filter h) with
      | None   -> aux t ret
      | Some e -> aux t (e::ret)
  in aux list []

let simplifie l clauses =
  (* à compléter *)
let  aux_filtre element liste = 
  match List.mem element liste with
  |true -> None
  |false -> match List.mem (-element) liste with
            |true -> Some ( List.filter (fun x -> (x <> -element)) liste)
            |false ->Some liste 

in  List.filter(fun x -> (x <> []) ) (filter_map (aux_filtre l) clauses )


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
      
      
      let () =
      Printf.printf "Original Clauses:\n";
      List.iter (fun clause -> Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int clause))) exemple_7_2;
    
      let l = 2 in
      let simplified_clauses = simplifie l exemple_7_2 in
    
      Printf.printf "\nSimplified Clauses (with l=%d):\n" l;
      List.iter (fun clause -> Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int clause))) simplified_clauses;      
      
      