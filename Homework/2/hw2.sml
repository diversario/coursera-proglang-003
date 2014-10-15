(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2



(* put your solutions for problem 1 here *)

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


(* 1d *)
fun similar_names(subs, full_name: {first:string,middle:string,last:string}) =
	let val {first=fname, middle=mname, last=lname} = full_name

		fun build_list (first_names, acc) =
			case first_names of
				[] => acc
			|	x :: xs => build_list (xs, acc @ [{first=x, middle=mname, last=lname}])
	in
		build_list (get_substitutions2(subs, fname), [full_name])
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


(* 2a *)
fun card_color (Clubs, _) = Black
  | card_color (Spades, _) = Black
  |	card_color _ = Red


(* 2b *)
fun card_value (_, Num n) = n
  | card_value (_, Ace) = 11
  |	card_value _ = 10


(* 2c *)
fun remove_card (cards, c, ex) =
    case cards of
        [] => raise ex
    |   x :: xs =>
        if x = c then
            xs
        else x :: remove_card(xs, c, ex)


(* 2d *)
fun all_same_color (cards) =
	case cards of
		x :: [] => true
	| 	x :: y :: xs => x = y andalso all_same_color(y :: xs)
		

(* 2e *)
fun sum_cards (cards) =
	let fun sum (cs, acc) =
		case cs of
			[] => acc
		|	x :: xs => sum (xs, card_value(x) + acc)
	in
		sum (cards, 0)
	end


(* 2f *)
fun score (held_cards, goal) =
	let val sum = sum_cards(held_cards)
		val is_preliminary = not (all_same_color(held_cards))
		val pre_score =
			if sum > goal then 3 * (sum - goal)
			else goal - sum
 	in
		if is_preliminary then pre_score
		else pre_score div 2
	end