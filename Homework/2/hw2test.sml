(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

(* 1a *)
val test_1a_1 = all_except_option("string", ["string"]) = SOME [];

val test_1a_2 = all_except_option("string", ["no_string"]) = NONE;

val test_1a_3 = all_except_option("string", ["foo", "string", "bar"]) = SOME ["foo", "bar"];

val test_1a_4 = all_except_option("string", []) = NONE;


(* 1b *)
val test_1b_1 = get_substitutions1([["foo"],["there"]], "foo") = [];

val test_1b_2 = get_substitutions1(
	[["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    "Fred"
) = ["Fredrick","Freddie","F"];

val test_1b_3 = get_substitutions1(
	[["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
    "Jeff"
) = ["Jeffrey","Geoff","Jeffrey"];

val test_1b_4 = get_substitutions1([[],["there"]], "foo") = [];

val test_1b_5 = get_substitutions1([], "foo") = [];


(* 1c *)
val test_1c_1 = get_substitutions2([["foo"],["there"]], "foo") = [];

val test_1c_2 = get_substitutions2(
	[["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    "Fred"
) = ["Fredrick","Freddie","F"];

val test_1c_3 = get_substitutions2(
	[["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
    "Jeff"
) = ["Jeffrey","Geoff","Jeffrey"];

val test_1c_4 = get_substitutions2([[],["there"]], "foo") = [];

val test_1c_5 = get_substitutions2([], "foo") = [];


(* 1d *)
val test_1d_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];

val test_1d_2 = similar_names([[], []], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}];


(* 2a *)
val test_2a_1 = card_color((Clubs, Num 2)) = Black;

val test_2a_2 = card_color((Hearts, Num 2)) = Red;


(* 2b *)
val test_2b_1 = card_value((Clubs, Num 2)) = 2;

val test_2b_2 = card_value((Clubs, Ace)) = 11;


(* 2c *)
val test_2c_1 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];

val test_2c_2 = (remove_card([(Clubs, Ace)], (Hearts, Ace), IllegalMove) = []; false)
	handle IllegalMove => true;

val test_2c_3 = (remove_card([], (Hearts, Ace), IllegalMove) = []; false)
	handle IllegalMove => true


(* 2d *)
val test_2d_1 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true;

val test_2d_2 = all_same_color([(Hearts, Ace), (Clubs, Ace)]) = false;

val test_2d_2 = all_same_color([(Hearts, Ace), (Hearts, Ace), (Hearts, Ace)]) = true;

val test_2d_3 = all_same_color([(Hearts, Ace)]) = true;

val test_2d_4 = all_same_color([(Hearts, Ace), (Diamonds, Num(2))]) = true;

val test_2d_5 = all_same_color([]) = true;


(* 2e *)
val test_2e_1 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4;

val test_2e_2 = sum_cards([(Clubs, Num 2),(Clubs, Ace)]) = 13;

val test_2e_3 = sum_cards([]) = 0;

val test_2e_4 = sum_cards([(Clubs, Num 2)]) = 2;


(* 2f *)
val test_2f_1 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;

val test_2f_2 = score([(Hearts, Ace),(Clubs, Ace)],10) = 36;

val test_2f_3 = score([(Hearts, Ace)],10) = 1;

val test_2f_4 = score([],10) = 5;


(* 2g *)
val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3;

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true);