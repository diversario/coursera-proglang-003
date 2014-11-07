(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

(* 1 *)
val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"];
val test1_2 = only_capitals ["A","Boo", "nope", "Car"] = ["A","Boo","Car"];
val test1_3 = only_capitals [] = [];

(* 2 *)
val test2_1 = longest_string1 ["A","bc","C"] = "bc";
val test2_2 = longest_string1 ["A","","C"] = "A";
val test2_3 = longest_string1 [] = "";
val test2_4 = longest_string1 ["A","bc","cd"] = "bc";

(* 3 *)
val test3_1 = longest_string2 ["A","bc","C"] = "bc";
val test3_2 = longest_string2 ["A","","C"] = "C";
val test3_3 = longest_string2 [] = "";
val test3_4 = longest_string2 ["A","bc","cd"] = "cd";

(* 4a *)
val test4a_1 = longest_string3 ["A","bc","C"] = "bc";
val test4a_2 = longest_string3 ["A","","C"] = "A";
val test4a_3 = longest_string3 [] = "";
val test4a_4 = longest_string3 ["A","bc","cd"] = "bc";

(* 4b *)
val test4b_1 = longest_string4 ["A","bc","C"] = "bc";
val test4b_2 = longest_string4 ["A","","C"] = "C";
val test4b_3 = longest_string4 [] = "";
val test4b_4 = longest_string4 ["A","bc","cd"] = "cd";
val test4b_5 = longest_string4 ["A","B","C"] = "C";

(* 5 *)
val test5_1 = longest_capitalized ["A","bc","C"] = "A";
val test5_2 = longest_capitalized ["A","bc", "Cd", "fgi"] = "Cd";

(* 6 *)
val test6 = rev_string "abc" = "cba";

(* 7 *)
val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;
val test7_2 = first_answer (fn x => if x = "a" then SOME x else NONE) ["v", "A", "a", "cs"] = "a";
val test7_3 = (
		first_answer (fn x => if x = "X" then SOME x else NONE) ["v", "A", "a", "cs"] = "a"; false
	) handle NoAnswer => true;

(* 8 *)
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE;
val test8_2 = all_answers (fn x => SOME [x]) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7];
val test8_3 = all_answers (fn x => SOME [x]) [[2, 21],[3, 31],[4],[5,6]] = SOME [[2, 21] ,[3, 31] ,[4],[5,6]];
val test8_4 = all_answers (fn x => SOME [[1,2]]) [[4],[5,6]] = SOME [[1,2],[1,2]];
val test8_5 = all_answers (fn x => SOME [[1,2]]) [] = SOME [];

(* 9a *)
val test9a_1 = count_wildcards Wildcard = 1;
val test9a_2 = count_wildcards (TupleP [Variable("a"), Variable("b")]) = 0;
val test9a_3 = count_wildcards (TupleP [Variable("a"), Wildcard]) = 1;
val test9a_4 = count_wildcards (TupleP [Wildcard, Wildcard]) = 2;

(* 9b *)
val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1;
val test9b_2 = count_wild_and_variable_lengths (TupleP [Variable("abc"), Wildcard]) = 4;

(* 9c *)
val test9c_1 = count_some_var ("x", Variable("x")) = 1;
val test9c_2 = count_some_var ("x", TupleP [Variable("x"), Variable("y"), Variable("x")]) = 2;

(* 10 *)
val test10_1 = check_pat (Variable("x")) = true;
val test10_2 = check_pat (TupleP [Variable("x"), Variable("y"), Variable("x")]) = false;
val test10_3 = check_pat (TupleP [Variable("x"), Variable("y"), Variable("z")]) = true;
val test10_4 = check_pat (ConstructorP ("hi", TupleP[Variable "x",Variable "x"])) = false;
val test10_5 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false;

(* 11 *)
val test11_1 = match (Const(1), UnitP) = NONE;
val test11_2 = match (Tuple [Unit, Const 3, Const 2], TupleP [Variable "1", ConstP 3, Wildcard]) = SOME [("1",Unit)];
val test11_3 = match (Tuple [Unit, Tuple [Const 3]], TupleP [Variable "1", TupleP [Variable "123"]]) = SOME [("1",Unit),("123",Const 3)];
val test11_4 = match (Tuple [Unit, Tuple [Const 3, Unit]], TupleP [Variable "1", TupleP [Variable "123", ConstP 3]]) = SOME [("1",Unit),("123",Const 3)];

(* 12 *)
val test12_1 = first_match Unit [UnitP] = SOME [];
val test12_2 = first_match (Const 3) [UnitP, ConstP 3] = SOME [];
val test12_3 = first_match (Const 3) [UnitP, Variable "f"] = SOME [("f", Const 3)];
val test12_4 = first_match (Const 3) [UnitP, TupleP[]] = NONE;
