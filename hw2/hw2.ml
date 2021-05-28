
(*Carson Kim ---- CS131 ---- HW#2 Source Code *)



(*Question 1: convert grammar*)

let rec prod_function rules start_symbol  = match rules with
  |[] -> []
  |head::tail -> match head with 
    |(lhs, rhs) -> if (lhs) = start_symbol then (rhs)::(prod_function tail start_symbol) 
        else prod_function tail start_symbol

let convert_grammar gram1 = 
match gram1 with
    (lhs, rhs) -> (lhs, (prod_function rhs))
  
(*Question 2: obtain list of leaves from tree*)

let rec parse_tree_list list = match list with 
  |[]-> []
  |head::tail -> match head with 
    |Node (lhs, subtree) -> (parse_tree_list subtree)@(parse_tree_list tail)
    |Leaf x ->  x::(parse_tree_list tail)
                   
let parse_tree_leaves tree = let x = [tree] in parse_tree_list x
    

let make_matcher gram = None

let make_parser gram = None