let test_grammar =
  (Gobble,
   function
     | Gobble->
         [[N Wobble; N Wob; N Gobble; N Dobble];
          [N Wobble]]
     | Wobble ->
	 [[N Gobble];
	  [N Hobble];
	  [T"wahhhh"; N Gobble; T"blahhhh"]]
     | Hobble>
	 [[T"boooo"; N Gobble]]
     | Wob ->
	 [[T"yahoo"]];
     | Dobble ->
	 [[T"hi"]; [T"yabadabadoo"]; [T"yoohoo"]; [T"hello???"]; [T"blahh"];]