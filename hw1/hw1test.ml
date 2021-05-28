let my_subset_test0 = subset [1] [1;2;3]
let my_subset_test1 = subset [3;1;2] [1;2;3]
let my_subset_test2 = not (subset [11;10;7] [10;9;7])

let my_equal_sets_test0 = equal_sets [2;2;4] [2;4]
let my_equal_sets_test1 = not (equal_sets ["smallberg"; "nachenberg"; "eggert"] ["smallberg";"smallberg"; "eggert"])

let my_set_union_test0 = equal_sets (set_union [] ["david";"carey"]) ["david"; "carey"]
let my_set_union_test1 = equal_sets (set_union [1; 2] [4;1;5;6]) [1;2;4;5;6]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_symdiff_test0 =
  equal_sets (set_symdiff [1;2;3] [3;4]) [1;2;4]
let my_set_symdiff_test1 =
  equal_sets (set_symdiff ["david"; "carey"] ["paul"; "carey"]) ["david"; "paul"]
let my_set_symdiff_test2 =
  equal_sets (set_symdiff ["smallberg"; "smallberg"] ["smallberg"; "smallberg"]) []

(*No test cases for Russel's Paradox Question. *)

let computed_fixed_point_test0 
= computed_fixed_point (=) (fun x -> x) 10 = 10
let computed_fixed_point_test1 =
  computed_fixed_point (=) sqrt 10. = 1.

(*test cases for filter_reachable *)
type ow_nonterminals = | Dva | Winston | Sigma | Zarya | Genji | Reaper
let ow_rules = 
  [	
    Reaper, [N Zarya; T "death blossom"; N Dva];
    Dva, [N Winston ];
    Winston, [N Sigma; N Genji];
    Sigma, [T"flux"];
    Zarya, [T "graviton surge"];
    Genji, [T"dragon blade"; N Dva];
  ]
  
let ow_grammar = Reaper, ow_rules

let ow_test01 = filter_reachable ow_grammar = ow_grammar
let ow_test02 = filter_reachable (Dva, ow_rules) =  (Dva, [Dva, [N Winston]; Winston,[N Sigma; N Genji];	Sigma, [T"flux"]; Genji, [T"dragon blade"; N Dva]])
let ow_test03 = filter_reachable (Zarya, ow_rules) = (Zarya, [Zarya, [T "graviton surge"]])
let ow_test04 = filter_reachable (Genji, List.tl ow_rules) = (Genji, [Dva, [N Winston ]; Winston, [N Sigma; N Genji]; Sigma, [T"flux"];
                                                                      Genji, [T"dragon blade"; N Dva]])

                                                    