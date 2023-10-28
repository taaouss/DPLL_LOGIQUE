let lyes litteral  = -litteral 
let pur clauses =
  let liste_concatinee = List.flatten clauses in
    let rec pur_aux list =
      match list with 
      |[]-> None
      |litteral:: reste_liste ->  match List.mem (-litteral) reste_liste  with 
                                   | true -> pur_aux ( List.filter (fun x -> (x <> -litteral) && (x <> litteral) ) reste_liste)
                                   | false-> Some litteral
    in pur_aux liste_concatinee
  


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
      
      
    let test_unitaire () = 
      let result = pur exemple_7_2 in
      match result with 
      |None -> Printf.printf "Aucun Résultat"
      |Some a -> Printf.printf "Résultat: %d" a
       
    
    
    
    let () = test_unitaire ()    