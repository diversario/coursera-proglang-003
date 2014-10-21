fun is_older (date1 : int*int*int, date2 : int*int*int) = 
	#1 date1 < #1 date2
	orelse
	(#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
	orelse
	(#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)
	
fun number_in_month (dates : (int*int*int) list, month : int) = 
	if null dates
	then 0
	else if #2 (hd dates) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) = 
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
	if null dates
	then []
	else if #2 (hd dates) = month
	then (hd dates)::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (xs : string list, n : int) =
	if n = 1
	then hd xs
	else get_nth(tl xs, n-1)

fun date_to_string (date : (int*int*int)) = 
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum (sum : int, integers : int list) =
	let fun accumulate(accum : int, xs : int list) =
		if(accum + (hd xs) >= sum)
		then 0
		else 1 + accumulate(accum + (hd xs), tl xs)
	in
		accumulate(0, integers)
	end

fun what_month (day : int) =
	let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(day, days_in_month) + 1
	end

fun month_range (day1 : int, day2 : int) =
	if day1 > day2
	then [] 
	else what_month(day1)::month_range(day1 + 1, day2)

fun oldest (dates : (int*int*int) list) =
	if null dates
	then NONE
	else
		let fun oldest_nonempty (dates : (int*int*int) list) = 
			if null (tl dates)
			then hd dates
			else
				let val current_oldest = oldest_nonempty(tl dates)
				in
					if is_older(hd dates, current_oldest)
					then hd dates
					else current_oldest
				end
		in
			SOME (oldest_nonempty dates)
		end

fun remove_duplicates (xs : int list) = 
	let
		fun remove (xs : int list, x : int) = 
			if null xs 
			then []
			else if (hd xs) = x
			then remove(tl xs, x)
			else (hd xs)::remove(tl xs, x)

	in
		if null xs
		then []
		else (hd xs)::remove_duplicates(remove(tl xs, (hd xs)))
	end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) = 
	number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
	dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : int*int*int) =
	let 
		val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
		fun is_leap_year (year : int) =
			(year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 > 0)
		fun get_nth (xs : int list, n : int) =
			if n = 1 
			then hd xs
			else get_nth(tl xs, n-1)
		fun get_max_day_in_month (month : int, year : int) =
			if month = 2 andalso is_leap_year(year)
			then 29
			else get_nth(days_in_month, month)
	in
		(#1 date > 0)
		andalso (#2 date >= 1 andalso #2 date <= 12)
		andalso
		(#3 date >= 1 andalso #3 date <= get_max_day_in_month(#2 date, #1 date))
	end
