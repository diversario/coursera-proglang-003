(* comment *)

val x = 34;
val y = 17;
val z = x + y;

val abs_of_z = if z < 0 then 0 - z else z;

fun sum_list (xs: int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

fun list_product (xs: int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)


