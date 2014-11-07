
(* Programming Languages (Coursera) : Homework #2 
   NOTE: Some functions missing
 *)

fun same_string (s1 : string, s2 : string) =
    s1 = s2



fun all_except_option (str: string, xs : string list) =
    let val lst = nil
        fun build_list(str: string, xs : string list, lst : string list) =
	    case xs of x::nil
		 => if same_string(str,x) then SOME lst else NONE
	    | nil 
	         => NONE
            | x::xs'  
	         => if same_string(str,x) then SOME (lst @ xs') 
		    else build_list(str,xs',lst@[x])
    in
	build_list(str,xs,lst)
    end

fun get_substitutions1 (subs : string list list, name : string) =
    case subs of nil => nil
		| xs::xss => 
		  case all_except_option(name,xs)
		    of NONE => get_substitutions1 (xss,name)
		    | SOME ys => ys @ get_substitutions1 (xss,name)

fun get_substitutions2 (subs : string list list, name : string) =
    let
	val lst = nil
	fun build_list(names : string list list, name : string, output : string list) =
	    case names of nil => nil
		|  xs::nil =>
		    (case all_except_option(name,xs)
			of NONE => output
			| SOME ys => output @ ys) 
		| xs::xss => 
		    (case all_except_option(name,xs)
			of NONE => build_list(xss,name,output)
			| SOME ys => build_list(xss,name,output @ ys))
    in
	build_list(subs,name,lst)
    end


fun similar_names (subs, full_name) =
    let
	val name_list = [full_name]
	fun get_subs {first=x,last=y,middle=z} = 
	    get_substitutions2(subs,x)
	fun new_list (sub_list,output_list,{first=x,last=y,middle=z}) =
	    case sub_list of
		nil => output_list
		| name::nil => output_list @ [{first=name,last=y,middle=z}]
		| name::names => new_list(names,(output_list @ 
			[{first=name,last=y,middle=z}]),full_name)
    in
	new_list(get_subs (full_name),name_list,full_name)
    end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

val card_list = [(Spades,Ace),(Hearts,Num 5),(Diamonds,Queen)]
val test_card = (Diamonds,Queen)

fun card_color (c : card) =
    case c of 
	  (Clubs,_) => Black
	| (Spades,_) => Black 
	| (Diamonds,_) => Red
	| (Hearts,_) => Red

fun card_value (c : card) =
    case c of 
	  (_,Ace) => 11
        | (_,Num x) => x
        | (_,_) => 10


fun remove_card (cs, c, e) =
    let val c_list = nil
	fun rem (ys,y,output_list) =
	    case ys of nil => raise e
		 | x::nil => if y = x then output_list
			else raise e
	         | x::xs' => if y = x then output_list @ xs'
			else rem (xs',c,(output_list @ [x]))

    in
	rem (cs,c,c_list)
    end

(* Missing some functions *)

fun sum_cards (cs) =
    let
	fun sumTR (xs,acc) = 
	    case xs of nil => acc
		| x::xs' => sumTR (xs',(card_value(x) + acc))
    in
	sumTR (cs,0)
    end

(* Missing some functions *)
