fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun remove (str, strings) =
	case strings of
		[] => []
	|	x :: xs =>
			if same_string(x, str) then
				remove(str, xs)
			else x :: remove(str, xs)


fun cons_opt (x, SOME xs) = SOME (x :: xs)
  | cons_opt (x, NONE) = NONE


fun remove2 (str, strings) =
	case strings of
		[] => NONE
	|	x :: xs =>
			if same_string(x, str) then
				SOME(xs)
			else cons_opt (x, remove2(str, xs))