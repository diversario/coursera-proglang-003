(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str : string, str_list : string list) =  
    let fun present(str_list) = 
	    case str_list of 
		[] => false 
	      | str1 :: str_list' => if same_string(str, str1) then true else present(str_list')
    in
	if present(str_list)
	then let fun xs(str_list) =
		     case str_list of
			 [] => []
		       | str1 :: str_list' => if same_string(str, str1) then xs(str_list') else str1 :: xs(str_list') 
	     in 
		 SOME (xs(str_list))
	     end
	else NONE
    end

fun get_substitutions1(subs : string list list, s : string) = 
    case subs of
	[] => []
      | list1 :: subs' => case all_except_option(s, list1) of
			      NONE => get_substitutions1(subs', s)
			    | SOME lst => lst @ get_substitutions1(subs', s)

fun get_substitutions2(subs : string list list, s : string) = 
    let fun helper(subs, s, acc) = 
		case subs of
		    [] => acc
		  | list1 :: subs' => case all_except_option(s, list1) of
					  NONE => helper(subs', s, acc)
					| SOME lst => helper(subs', s, lst @ acc)
    in
	helper(subs, s, [])
    end

type full_name = {first:string, middle:string, last:string}

fun similar_names(subs, fullname) = 
    let val {first = f, middle = m , last = l} = fullname
	fun names(first_names) = 
		 case first_names of
		     [] => []
		   | name :: first_names' => {first = name, middle = m, last = l} :: (names(first_names'))
    in 
	fullname :: names(get_substitutions2(subs, f))
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

fun card_color(suit, _) =
    case suit of 
	(Clubs | Spades) => Black 
      | _ => Red

fun card_value(_, value) =
    case value of
	Num i => i
     | Ace => 11
     | _ => 10 

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
     | cs1 :: cs' => if (cs1 = c) then cs' else cs1 :: remove_card(cs', c, e)

fun all_same_color cs = 
    case cs of
	[] => true
      | cs1 :: [] => true
      | cs1 :: cs2 :: cs' => card_color(cs1) = card_color(cs2) andalso all_same_color(cs') 

fun sum_cards cs =
    let fun adder(cs, acc) = 
	    case cs of
		[] => acc
	     | cs1 :: cs' => adder(cs', acc + card_value(cs1))
    in
	adder(cs, 0)
    end

fun score(cs, goal) = 
    let val sum = sum_cards cs
	val prelim = if sum > goal then 3 * (sum - goal) else goal - sum
    in
	if all_same_color cs then prelim div 2 else prelim
    end

fun officiate(cs, ms, goal) = 
    let fun helper(cards : card list, held : card list, moves : move list, goal : int) =
	    case (cards, held, moves, goal) of
		(_,     _, [],    _) => score (held, goal)
	      | ([],    _, _,     _) => score (held, goal)
	      | (c::cs, _, m::ms, _) => case m of
				      Discard d => helper(c::cs, remove_card (held, d, IllegalMove), ms, goal)
				    | Draw => case c::cs of
						  [] => score (held, goal)
						  | _ => 
						    let
							val held' = c::held
							val held_sum = sum_cards (held')
						    in
							if (held_sum > goal)
							then score (held', goal)
							else helper(cs, held', ms, goal)
						    end
    in
	helper(cs, [] , ms, goal)
    end

