(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* all_except_option returns NONE if toExcept is not in the strings list,
   otherwise it returns SOME list without the first occurrence of toExcept *)
fun all_except_option (toExclude, strings) =
    case strings of
	[] => NONE
      | hds::tls => if same_string(hds, toExclude)(* first occurrence found *)
		    then SOME tls
		    else
			case all_except_option(toExclude, tls) of
			    NONE => NONE (* toExclude never found *)
			 | SOME lst => SOME(hds::lst)

(* takes a list of equivalency lists, such as ["Charles", "Charlie", "Chuck"],
and returns a list of all items equivalent to the given base, from among all
equivalency lists *)
fun get_substitutions1 (substitutions, base) =
    case substitutions of
	[] => []
      | hds::tls => case all_except_option(base, hds) of
			NONE => get_substitutions1(tls, base)
		      | SOME lst => lst @ get_substitutions1(tls, base)

(* a tail-recursive version of get_substitutions1 *)
fun get_substitutions2 (substitutions, base) =
    let
	fun tail_recurse (accumulated, [], base) = accumulated
	  | tail_recurse (accumulated, hds::tls, base) = 
	    case all_except_option(base, hds) of
		NONE => tail_recurse(accumulated, tls, base)
	      | SOME lst  => tail_recurse(lst @ accumulated, tls, base)
    in
	tail_recurse([], substitutions, base)
    end

(* returns a list of name records, including all valid first names as
   determined by the substitutions provided. Middle and last names remain
   unchanged. *)
fun similar_names (substitutions, fullname) =
    let
	val {first=first, middle=middle, last=last} = fullname
	val first_name_options = get_substitutions1(substitutions, first)
        (* get_fullname_list returns a list of name records *)
	fun get_fullname_list (first_name_options : string list) = 
	    case first_name_options of
		[] => []
	      | hds::tls => {first=hds, middle=middle, last=last} ::
			    get_fullname_list(tls)
    in
	(* include the original full name in the result *)
	fullname::get_fullname_list(first_name_options)
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

(* returns the color of the card given *)
fun card_color (Clubs, rank) = Black
  | card_color (Diamonds, rank) = Red
  | card_color (Hearts, rank) = Red
  | card_color (Spades, rank) = Black

(* returns the value of the card given, according to the game rules *)
(* do not expect a full sequential increasing order *)
fun card_value (suit, Num value) = value
| card_value (suit, Ace) = 11
| card_value (suit, rank) = 10

(* returns the given list of cards, with the toRemove card removed *)
(* if toRemove card not located, the exception passed in is raised *)
fun remove_card (cards, toRemove, unfoundException) =
    case cards of
	[] => raise unfoundException
      | hds::tls => if hds = toRemove
		    then tls
		    else hds::remove_card(tls, toRemove, unfoundException)

(* returns false if cards contains both red and black cards, otherwise true *)
fun all_same_color (cards) =
    case cards of
	first::second::tls (* at least two cards *)
	    => card_color(first) = card_color(second)
	       andalso all_same_color(second::tls)
      | _ (* zero or one cards, so vacuously true *)
	    => true

(* return the sum of all cards' values *)
fun sum_cards (cards) =
let
    fun tail_recurse (sum, []) = sum
      | tail_recurse (sum, hds::tls) = tail_recurse(sum + card_value(hds),
						    tls)
in
    tail_recurse(0, cards)
end

(* helper function, used in score and score_challenge *)
(* returns the preliminary score, based on the goal and the gross score *)
fun get_preliminary_score (gross_score, goal) =
    (* only a gross_score of exactly 0 gets a perfect 0 score *)	      
    if gross_score > goal
    then 3 * (gross_score - goal)
    else goal - gross_score

(* helper function, used in score and score_challenge *)
(* returns the final score, based on the preliminary score, and whether the
held cards are all of the same color or not *)
fun get_final_score (preliminary_score, cards) =
    if all_same_color(cards) (* receive an improved score *)
    then preliminary_score div 2
    else preliminary_score

(* returns the score for a player's hand at game end, based on the set goal *)
fun score (cards, goal) =
let
    val gross_score = sum_cards(cards)
    val preliminary_score = get_preliminary_score(gross_score, goal)
in
    get_final_score(preliminary_score, cards)
end

(* runs this solitaire game resulting from the given card list and list of
   move choices *)
fun officiate (cards, moves, goal) =
let
    fun process_moves (held, cards, [], goal) = score(held, goal)
      | process_moves (held, cards, Discard toDiscard::tlMoves, goal) =
        process_moves (remove_card(held, toDiscard, IllegalMove), cards,
		       tlMoves, goal)
      | process_moves (held, cards, Draw::tlMoves, goal) =
	case cards of
	    [] (* no more cards to draw from, game over *)
	    => score(held, goal)
	  | hdCards::tlCards (* draw head card *)
	    => 
	    let
		val new_hand = hdCards::held
		val running_score = score(new_hand, goal)
	    in
		if running_score > goal
		then running_score (* goal exceeded, game over *)
		else process_moves(new_hand, tlCards, tlMoves, goal)
	    end
in
    process_moves([], cards, moves, goal) (* start with empty hand *)
end

fun score_challenge (cards, goal) =
let
    (* accumulates ace count to then determine best score *)
    fun prelim_challenge (sum, ace_count, (suit,Ace)::tlCards, goal) =
	prelim_challenge (sum, ace_count + 1, tlCards, goal)
      | prelim_challenge (sum, ace_count, hdCard::tlCards, goal) = 
	prelim_challenge (sum + card_value(hdCard), ace_count, tlCards, goal)
      (* base case, where we determine how to count aces for best score *)
      | prelim_challenge (sum, ace_count, [], goal) = 
	if sum + ace_count >= goal (* use all aces as 1's *)
	then get_preliminary_score(sum + ace_count, goal)
	else
	    let
                (* deterime how much below goal if all aces are 1's *)
		val under_shoot = goal - (sum + ace_count)
                (* determine best number of aces to use as 11's *)
		val max_enlarged_aces = (under_shoot div 10)
					+ ((under_shoot mod 10) div 8)
		val ace_contribution = 
		    if max_enlarged_aces < ace_count
		    then ace_count + (10 * max_enlarged_aces)
		    else ace_count + (10 * ace_count)
	    in
		get_preliminary_score(sum + ace_contribution, goal)
	    end

    val preliminary_score = prelim_challenge(0, 0, cards, goal)
in
    get_final_score(preliminary_score, cards)
end

(* just like officiate, except uses the score_challenge scoring scheme *)
fun officiate_challenge (cards, moves, goal) =
let
    fun process_moves (held, cards, [], goal) = score_challenge(held, goal)
      | process_moves (held, cards, Discard toDiscard::tlMoves, goal) =
        process_moves (remove_card(held, toDiscard, IllegalMove), cards,
		       tlMoves, goal)
      | process_moves (held, cards, Draw::tlMoves, goal) =
	case cards of
	    [] (* no more cards to draw from, game over *)
	    => score_challenge(held, goal)
	  | hdCards::tlCards (* draw head card *)
	    => 
	    let
		val new_hand = hdCards::held
		val running_score = score_challenge(new_hand, goal)
	    in
		if running_score > goal
		then running_score (* goal exceeded, game over *)
		else process_moves(new_hand, tlCards, tlMoves, goal)
	    end
in
    process_moves([], cards, moves, goal) (* start with empty hand *)
end

fun careful_player (cards, goal) =
let
    fun make_moves (moves, held, [], goal) = moves
      | make_moves (moves, held, hdCard::tlCards, goal) =
	let
	    val sum_held = sum_cards(held)
	    val under_shoot = goal - sum_held
	    fun value_is_held ([], value) = false
	      | value_is_held (hdCard::tlCards, value) = 
		if card_value(hdCard) = value
		then true
		else value_is_held(tlCards, value)
	    fun	upcoming_zero (held, card, under_shoot) = 
		value_is_held(held, card_value(card) - under_shoot)
	in
	    if under_shoot > 10
	    then make_moves(moves @ [Draw], hdCard::held, tlCards, goal)
	    else if under_shoot = 0
		    orelse not (upcoming_zero(held, hdCard, under_shoot))
	    then moves
	    else (*  upcoming_zero *)
		let
		    fun find_card_of_value (hdCard::tlCards, value) = 
			if card_value(hdCard) = value
			then hdCard
			else find_card_of_value(tlCards, value)
		    val discardedValue = card_value(hdCard) - under_shoot
		    val toDiscard = find_card_of_value(held, discardedValue)
		    val newMoves = moves @ (Discard toDiscard)::[Draw]
		in
		    newMoves
		end
	end
in
    make_moves([], [], cards, goal)
end
