(* Programming Languages - Assignment 1 *)

(* 1. Returns true if the first date comes before the second, false otherwise. *)
fun is_older (date1:int*int*int , date2:int*int*int) =
    ((#1 date1)<(#1 date2)) 
    orelse (((#1 date1)=(#1 date2))andalso((#2 date1)<(#2 date2))) 
    orelse (((#1 date1)=(#1 date2))andalso((#2 date1)=(#2 date2))andalso((#3 date1)<(#3 date2)))

(* 2. Returns how many of the given dates belong to the given month. *)
fun number_in_month (dates:(int*int*int)list , month:int) =
    if null dates
    then 0
    else if (#2 (hd dates) = month)
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

(* 3. Returns how many of the given dates belong to any of the given months. *)
fun number_in_months (dates:(int*int*int)list , months:int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* 4. Returns a list with the given dates that belong to the given month. *)
fun dates_in_month (dates:(int*int*int)list , month:int) =
    if null dates
    then []
    else if (#2 (hd dates) = month)
    then (hd dates)::dates_in_month (tl dates, month)
    else
    dates_in_month (tl dates, month)

(* 5. Returns a list with the given dates that belong to any of the given months. *)
fun dates_in_months (dates:(int*int*int)list , months:int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* 6. Returns the n-th element of the given list. *)
fun get_nth (strlist:string list , n:int) =
    if (null strlist) orelse (n=1)
    then hd strlist
    else get_nth (tl strlist, n-1)

(* 7. Returns the given date in string format. *)
fun date_to_string (date:int*int*int) =
    let
        val months_list = ["January ", "February ", "March ", "April ",
                           "May ", "June ", "July ", "August ",
                           "Semptember ", "October ", "November ", "December "]
    in
        (get_nth (months_list,#2 date)) ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(* 8. Returns the amount of integers in the given list required to reach a sum equal to the given int *)
fun number_before_reaching_sum (sum:int , xs:int list) =
    if (hd xs) >= sum
    then 0
    else 1 + number_before_reaching_sum (sum-hd xs, tl xs)

(* 9. Returns the number of the month that the given day belongs to. *)
fun what_month (day:int) =
    let
        val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum (day, days_per_month) + 1
    end

(* 10. Returns a list with the months contained between the given two days. *)
fun month_range (day1:int , day2:int) =
    if day1 > day2
    then []
    else what_month (day1)::month_range (day1 + 1, day2)

(* 11. Returns the oldest date in the provided list as an option, and NONE if the list is empty. *)
fun oldest (dates:(int*int*int)list) =
    if null dates
    then NONE
    else
        let
            fun oldest_nonempty (dates:(int*int*int)list) =
                if null (tl dates)
                then hd dates
                else
                    let
                        val temp_oldest = oldest_nonempty (tl dates)
                    in
                        if is_older (hd dates, temp_oldest)
                        then hd dates
                        else temp_oldest
                    end
        in
            SOME (oldest_nonempty dates)
        end

(* 12a. Returns true if the given list contains the given integer, false otherwise. This is a helper function for problems 12 and 13. *)
fun isMemberOf (x:int , xs:int list) =
    if null xs
    then false
    else if x = hd xs
    then true
    else isMemberOf (x, tl xs)

(* 12b. Removes any duplicates from a list of integers. This is a helper function for problem 12. *)
fun remove_duplicates (xs: int list) =
    if null xs
    then []
    else if isMemberOf (hd xs, tl xs)
    then remove_duplicates (tl xs)
    else hd xs::remove_duplicates (tl xs)

(* 12c. Repeats problem 3 after removing duplicate months. *)
fun number_in_months_challenge (dates:(int*int*int)list , months:int list) =
    number_in_months (dates, remove_duplicates(months))

(* 12d. Repeats problem 5 after removing duplicate months. *)
fun dates_in_months_challenge (dates:(int*int*int)list , months:int list) =
    dates_in_months (dates, remove_duplicates(months))

(* 13. Determines weather or not a date belongs to this world. *)
fun reasonable_date (date:(int*int*int)) =
    (#1 date > 0)
    andalso (#2 date > 0)
    andalso (#2 date < 13)
    andalso (#3 date > 0)
    andalso (isMemberOf(#2 date,[1,3,5,7,8,10,12]) andalso (#3 date < 32)
             orelse isMemberOf(#2 date,[4,6,9,11]) andalso (#3 date < 31)
             orelse ((#1 date mod 400) = 0
                     orelse (#1 date mod 4) = 0
                     andalso (not((#1 date mod 100) = 0)))
                     andalso (#3 date < 30)
             orelse (#3 date < 29))
