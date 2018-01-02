
(* Some functions for displaying colourings *)

(* Printing a list using a function for printing items in the list *)
let printlist item_printer l =
  let rec printlist_aux l =
    match l with
    | [] -> Printf.printf "%c" ']'
    | (h::t) -> Printf.printf "%s" ", ";
      item_printer h;
      printlist_aux t
  in (Printf.printf "%c" '[';
      match l with
      | (h::t) -> item_printer h; printlist_aux t
      | _ -> printlist_aux l)

(* A function for displaying an integer item *)
let int_printer i = Printf.printf "%d" i

(* A function for displaying a colour (represented by a string) *)
let show_color c = Printf.printf "%s" c

(* A function for displaying a node, colour pair *)
let show_node_and_color (n,c) =
  Printf.printf "(%d," n; show_color c; Printf.printf ")"

(* A function for showing a (complete) colouring *)
let show_coloring l =
  Printf.printf "\nColoring for the graph: "; printlist show_node_and_color l;
  Printf.printf "\n"
    

exception Search_Failure




  (* 
Checks list for a partucular key "k" and parses out other elements in key:
If key not found return None
 
  *)
  
let rec parse_list k l =
  match l with
  | [] -> None
  | (h,t):: l1 -> if k = h then (Some t) else parse_list k l1

(*
Takes lists and returns some (h,t) where h is a partcular color and t is the rest of colors
*)
      
let rec choose_color c =
  match c with
  |[] -> None
  |(h::t) -> Some (h,t)



(*
Checks all adjacents nodes for coloring. If particilar color c is used be return False else True
*)
     
let rec check_collision c n b =
  match n with
  |[]-> true
  |(h::t) -> match (parse_list h b) with
    |None -> check_collision c t b
    |Some x -> if (c = x) then false else check_collision c t b


(*
Checks if we can color a node a particular color 
*)

let rec color_valid n c a b =
  match (parse_list n a) with
  |None -> true
  |Some n -> check_collision c n b
	
     

  
let ask_user printer config =
  printer config;
  Printf.printf "More solutions (y/n)? ";
  if (read_line () = "y")
  then (raise Search_Failure)
  else ()


(* 
Function that takes in a list of nodes, a pair list containing the node and a list of its neighbors 
   and a list of colors and returns a colored list if it is possible to color the nodes with 
   given colors else it lets the user know that coloring is not possible. 
*)
    
let color_graph nodes adjacency colors =
  let rec color_graph_aux nodes colored =
    let rec color_rest n1 n2 color =
      match (choose_color color) with
      |None -> raise Search_Failure
      |Some(a1,a2) -> if (color_valid n1 a1 adjacency colored)
	then try (color_graph_aux n2 ((n1,a1)::colored)) with
	  Search_Failure -> color_rest n1 n2 a2
	else color_rest n1 n2 a2
    in match nodes with
    |[] -> ask_user show_coloring colored
    |(h::t) -> color_rest h t colors
  in try (color_graph_aux nodes []) with
            Search_Failure -> Printf.printf "\nNo (more) colourings possible\n"


     

      


let ask_user_cps printer config succ fail =
  printer config;
  Printf.printf "More solutions (y/n)? ";
  if (read_line () = "y") then (fail ()) else (succ ())


(*
Same Precondition and Invariant as color_graph , but  we use continuations to verify valid graph coloring 
*)
    
let color_graph_cps nodes adjacency colors =
  let rec color_graph_aux nodes colored succ fail =
    let rec color_node_remain n1 n2 color =
      match (choose_color color) with
      |None -> fail ()
      |Some(a1,a2)-> if (color_valid n1 a1 adjacency colored)
	then color_graph_aux n2 ((n1,a1)::colored) succ
	  (fun () -> color_node_remain n1 n2 a2)
	else color_node_remain n1 n2 a2
    in match nodes with
    |[] -> ask_user_cps show_coloring colored succ fail
    |(h::t) -> color_node_remain h t colors
  in color_graph_aux nodes [] (fun () -> ())
                                 (fun () -> Printf.printf "\nNo (more) colourings\n")