
(*Carson Kim --- HW #1 ----- CS131*)


(*Question 1: is a a subset of b*)
let rec subset a b = match a with
  |[]-> true;
  |a::tail -> if List.mem a b then subset tail b else false 
(*Question 2: Are the subsets equal? *)

let equal_sets a b = 
  let a = List.sort_uniq compare a in
  let b = List.sort_uniq compare b in
  if a = b then true else false

(*Question 3: Find the union of the sets*)

(*helper function for set union *)
let rec compute_union a b = 
  match a with
  | [] -> b
  | x::tail -> x::(compute_union tail b) 

(*sort out duplicates *)
let set_union a b =
  List.sort_uniq compare (compute_union a b)

(*Question 4: disjunctive union*)

(*helper function for disjunctive union*)
let rec set_symdiff_helper unionList a b = 
  match unionList with
  |[] -> []
  |x::tail -> if List.mem x a && List.mem x b then set_symdiff_helper tail a b 
      else List.append [x] (set_symdiff_helper tail a b)
          
let set_symdiff a b =
  set_symdiff_helper (set_union a b) a b

(*Question 5: Russel's Paradox*)
let self_member s = ()

(*It is not possible to write such a function in Ocaml, due to the way type-interfacing works in the language. If, for example, in the function self_member 
we passed in a list of some arbitrary type, and that list contained itself, then it seems as if the type of that list would be invalid - there's no way of determining its type based on these properties of self-membership, and would
likely cause problems in this particular language as it is very stringent with types. *)

(* Question 6 - computed fixed point *)
let rec computed_fixed_point eq f x =
  if (eq(x) (f x)) then x else computed_fixed_point eq f (f x);;


(*Question 7 - filter_reachable grammar*)

 type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec removeTFromList list = 
  match list with 
  |[] -> []
  |head::tail -> match head with 
    |T x -> removeTFromList tail 
    |N x -> x::removeTFromList tail
              
let rec removeAllT ruleList = 
  match ruleList with 
  |[] -> []
  |head::tail -> match head with 
    |(lhs, rhs) -> (lhs, removeTFromList rhs)::removeAllT(tail)
                     
                     
let rec remove_self_list symbol list = 
  match list with 
  |[]-> []
  |head::tail -> if head = symbol then remove_self_list symbol tail
      else head::remove_self_list symbol tail
             
  
let rec matchNonT start_symbol rules = match rules with 
  |[] -> []
  |head::tail -> match head with 
    |(lhs, rhs) -> if lhs = start_symbol then 
          if List.mem lhs rhs then let x = remove_self_list start_symbol rhs in 
            List.sort_uniq compare (matchNonT start_symbol tail@x)
          else List.sort_uniq compare (matchNonT start_symbol tail@rhs)
        else (matchNonT start_symbol tail)
  
let rec diff a b = match a with 
  |[]->[] 
  |head::tail-> if List.mem (List.hd a) b then diff tail b 
      else head::diff tail b
             
  
let rec obtainAllNonT list rules newList = match list with 
  |[] -> newList
  |head::tail -> let values = diff (matchNonT head rules) newList in 
      obtainAllNonT(tail@values) rules (newList@values)
        
  
let rec remove_self_list symbol list = 
  match list with 
  |[]-> []
  |head::tail -> if head = symbol then remove_self_list symbol tail
      else head::remove_self_list symbol tail
             
             
let rec reachableRules list non_list = match list with 
  |[] -> []
  |head::tail -> match head with 
    |(lhs, rhs) -> if (List.mem lhs non_list) then 
          head::(reachableRules tail non_list) 
        else reachableRules tail non_list 
             
  
let filter_reachable g = let rules = removeAllT (snd g) in
  let n_list = obtainAllNonT ([(fst g)]) rules ([(fst g)]) in 
  fst g, (reachableRules (snd g) n_list)
  
  
