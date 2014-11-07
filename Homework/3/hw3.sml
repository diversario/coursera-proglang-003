(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals strings =
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) strings


(* 2 *)
fun longest_string1 strings =
	foldl (fn (str, prev) => if String.size(str) > String.size(prev)
			then str else prev) "" strings


(* 3 *)
fun longest_string2 strings =
	foldl (fn (str, prev) => if String.size(str) >= String.size(prev)
			then str else prev) "" strings


(* 4 *)
fun longest_string_helper predicate =
	fn strings => foldl (fn (str, prev) =>
		if predicate(String.size(str), String.size(prev))
			then str
			else prev
		)
	"" strings

val longest_string3 = longest_string_helper (fn (x1, x2) => x1 > x2)

val longest_string4 = longest_string_helper (fn (x1, x2) => x1 >= x2)


(* 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
val rev_string = String.implode o rev o String.explode


(* 7 *)
fun first_answer f =
	let fun test items =
		case items of
			[] => raise NoAnswer
		|	x :: xs => case f(x) of
				SOME v => v
			|	NONE   => test(xs)
	in
		test
	end


(* 8 *)
fun all_answers f =
	let 
		fun append (lst, lists_opt) =
			case lists_opt of
				SOME v => lst @ v
			|	NONE => lst

		fun iterate items =
			case items of
				[] 		=> SOME []
			|	x :: xs => case f(x) of
					NONE     => NONE
				|	SOME lst => SOME(append(lst, iterate(xs)))
	in
		iterate
	end


(* 9a *)
fun count_wildcards p =
	let
		fun f1 () = 1
		fun f2 x = 0
	in
		g f1 f2 p
	end


(* 9b *)
fun count_wild_and_variable_lengths p =
	let
		fun f1 () = 1
		fun f2 x = String.size x
	in
		g f1 f2 p
	end


(* 9c *)
fun count_some_var (str, p) =
	let
		fun f1 () = 0
		fun f2 x = if x = str then 1 else 0
	in
		g f1 f2 p
	end


(* 10 *)
fun get_str p =
	case p of
		Variable v => [v]
	|	TupleP ps => List.foldl (fn (p,i) => get_str(p) @ i) [] ps
	|	ConstructorP (s, v) => get_str v
	|	_ => []

fun all_unique lst =
	case lst of
		[] => true
	|	x :: xs => not (List.exists (fn el => el = x) xs) andalso all_unique xs

fun check_pat p =
	all_unique(get_str p)


(* 11 *)
fun match (valu, pattern) =
	case (valu, pattern) of
		(_, Wildcard) => SOME []
	|	(_, Variable s) => SOME [(s, valu)]
	|	(Unit, UnitP) => SOME []
	|	(Const q, ConstP n) =>  if q = n then SOME [] else NONE
	|	(Constructor(s2, v), ConstructorP(s1, p)) =>
		if s1 = s2 then match(v, p)
		else NONE
	|	(Tuple vs, TupleP ps) =>
		if List.length(vs) = List.length(ps) then
			(* For some reason, if I just use ListPair.zip inline it won't compile *)
			let val pairs_list = ListPair.zip(vs, ps)
			in all_answers match pairs_list
			end
		else NONE
	|	_ => NONE


(* 12 *)
fun first_match v =
	fn plist =>
		let val pairs = List.map (fn p => (v, p)) plist
		in (SOME (first_answer match pairs)) handle NoAnswer => NONE
		end