(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* 1a *)
fun cons_opt (x, SOME xs) = SOME (x :: xs)
  | cons_opt (x, NONE) = NONE

fun all_except_option (str, strings) =
	case strings of
		[] => NONE
	|	x :: xs =>
			if same_string(x, str) then
				SOME(xs)
			else cons_opt (x, all_except_option(str, xs))


(* 1b *)
fun get_substitutions1 (subs, str) =
	case subs of
		[] => []
	|	x :: xs =>
			case all_except_option(str, x) of
				NONE => [] @ get_substitutions1(xs, str)
			|	SOME lst => lst @ get_substitutions1(xs, str)


(* 1c *)
fun get_substitutions2 (subs, str) =
	let fun f (xs, acc) =
		case xs of
			[] => acc
		|	x :: xs =>
			case all_except_option(str, x) of
				NONE => f(xs, acc)
			|	SOME lst => f(xs, acc @ lst)
	in
		f(subs, [])
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
