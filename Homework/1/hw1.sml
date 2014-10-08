(* 1 *)
fun is_older (date1: int * int * int, date2: int * int * int) =
	#1 date1 < #1 date2
	orelse
	#1 date1 = #1 date2 
		andalso #2 date1 < #2 date2
	orelse
		#1 date1 = #1 date2
		andalso #2 date1 = #2 date2
		andalso #3 date1 < #3 date2


(* 2 *)
fun number_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then 0
	else
		if #2 (hd dates) = month
		then 1 + number_in_month (tl dates, month)
		else 0 + number_in_month (tl dates, month)


(* 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months orelse null dates
	then 0
	else number_in_months(dates, tl months) + number_in_month(dates, hd months)


(* 4 *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then []
	else
		let fun is_in_month(date: (int*int*int)) =
			#2 date = month
		in
			if is_in_month(hd dates) = true
			then hd dates :: dates_in_month(tl dates, month)
			else dates_in_month(tl dates, month)
		end


(* 5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
	if null dates orelse null months
	then []
	else dates_in_month(dates, hd months) @  dates_in_months(dates, tl months)


(* 6 - don't know how to raise exceptions, so returning "" instead *)
fun get_nth (strings: string list, n: int) =
	if n = 1
	then if null strings
		then ""
		else hd strings
	else get_nth(tl strings, n - 1)


(* 7 *)
fun date_to_string (date: int*int*int) = 
	let
		val months = ["January", "February", "March",
			"April", "May", "June",
			"July", "August", "September",
			"October","November", "December"
		]

		val year = Int.toString(#1 date)
		val month = get_nth(months, #2 date)
		val day = Int.toString(#3 date)
	in
		month ^ " " ^ day ^ ", " ^ year
	end


(* 8 - infinite loop when sum is larger than sum of numbers *)
fun number_before_reaching_sum (sum, numbers) =
	let
		fun get_sum (nums, elements) = 
			if elements <= 0 orelse null nums
			then 0
			else hd nums + get_sum(tl nums, elements - 1)

		fun count (counter) =
			if get_sum(numbers, counter + 1) < sum
			then count(counter+1)
			else counter
	in
		count(0)
	end


(* 9 *)
fun what_month (day) =
	number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1


(* 10 *)
fun month_range (day1, day2) =
	let fun foo (n, range) =
		if n > day2
		then range
		else
			what_month(n) :: foo(n+1, range)
	in
		if day1 > day2
		then []
		else foo(day1, [])
	end


(* 11 *)
fun oldest (dates) =
	let fun find_oldest (list_of_dates, oldest) =
		if null list_of_dates
		then oldest
		else
			if is_older (hd list_of_dates, oldest)
			then find_oldest (tl list_of_dates, hd list_of_dates)
			else find_oldest (tl list_of_dates, oldest)
	in
		if null dates
		then NONE
		else SOME(find_oldest(tl dates, hd dates))
	end


(* 12 *)
fun contains (in_list, el) =
	if null in_list
	then false
	else
	if hd in_list = el
	then true
	else contains(tl in_list, el)

fun dedup(dup_list) =
	if null dup_list
	then []
	else
		if contains (tl dup_list, hd dup_list)
		then dedup (tl dup_list)
		else hd dup_list :: dedup (tl dup_list)

fun number_in_months_challenge (dates, months) =
	number_in_months(dates, dedup(months))

fun dates_in_months_challenge (dates, months) =
	dates_in_months(dates, dedup(months))


(* 13 *)
fun is_leap (year) =
	year mod 100 <> 0
	andalso (year mod 400 = 0 orelse year mod 4 = 0)

fun get_max_days (month, is_leap) =
	if month < 1 orelse month > 12 then ~1
	else
	let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		fun get_days (day_list, count) =
			if count = month then hd day_list
			else get_days(tl day_list, count + 1)

		val max_days = get_days(days, 1)
	in
		if month = 2 andalso is_leap
		then 29
		else max_days
	end

fun reasonable_date (date: int*int*int) =
	let
		val year = #1 date
		val month = #2 date
		val day = #3 date
		val max_days = get_max_days(month, is_leap(year))
	in
		if year <= 0 then false else
		if month < 1 orelse month > 12 then false else
		day >= 1 andalso day <= max_days 
	end