(*ex. 1*)
fun is_older ( date1 : int*int*int, date2 : int*int*int) =
    (* if year of the date1 is greater then year of the date2 then date1 is older then date2 *)
    if #1 date1 >  #1 date2 then false
    (* if year of the date1 is lesser then year of the date2 then date2 is older then date1 *)
    else if #1 date1 < #1 date2  then true

    (* now we know years are equal
    so we check relation between months
     *)	  
    else if #2 date1 > #2 date2 then false
    else if #2 date1 < #2 date2 then true

    (* no we know months are equal so we chck date *)
    else if #3 date1 < #3 date2 then true
    else false

(*ex. 2*)
fun number_in_month (dates: (int*int*int) list, month: int) =
	let
		fun count (ds: (int*int*int) list, acc: int) : int =
			if null ds then acc
			else if #2 (hd ds) = month then
				count(tl ds,acc+1)
			else
				count(tl ds,acc)
	in
		count(dates,0)
	end

(*ex. 3*)
fun number_in_months (dates: (int*int*int) list, months: int list) : int = 
	let	
		fun count (ds: (int*int*int) list, ms: int list, acc: int) =
			if null ms then acc
			else if null ds then acc 
			else count(ds,tl ms, acc+number_in_month(ds,hd ms))
	in
		count(dates,months,0)
	end 

(*ex. 4*)
 fun dates_in_month (dates: (int*int*int) list, month: int) : (int*int*int) list =
	let
		fun find (ds: (int*int*int) list, acc: (int*int*int) list) : (int*int*int) list=
        	if null ds then acc
			else if #2 (hd ds) = month then
            	find(tl ds,acc@((hd ds)::[]))
            else
            	find(tl ds,acc)
    in
    	find(dates,[])
    end 

(*ex 5*)
fun dates_in_months (dates: (int*int*int) list, months: int list) : (int*int*int) list =
	let
		fun find (ds: (int*int*int) list, ms: int list, acc: (int*int*int) list) : (int*int*int) list=
			if null ds then acc
			else if null ms then acc
			else find(ds,(tl ms),(acc@(dates_in_month(ds,(hd ms)))))
	in
		find(dates,months,[])
	 end

(*ex 6*)
fun get_nth (strings: string list, n: int) =
	let 
		fun iter (ss: string list, i : int)  =
			if i=n then hd ss
			else iter(tl ss,i+1)
	in
		iter(strings,1)
	end

(*ex 7*)
fun date_to_string(date: (int*int*int)) =
	let
		val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
	in
		(get_nth(months,#2 date))^" "^(Int.toString(#3 date))^", "^(Int.toString(#1 date))
	end

(*ex 8*)
fun number_before_reaching_sum(sum: int, intList: int list) = 
	let 
		fun sum_list (is: int list,n : int, acc: int) = 
			if null is then n
			else if ((hd is)+acc)>=sum then n
			else sum_list((tl is),n+1,acc+(hd is))
	in
		sum_list(intList,0,0)
	end

(*ex 9*)
fun what_month(day: int) = 
	let 
		val ndays_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(day,ndays_in_month)+1
	end

(*ex 10*)
fun month_range(day1: int, day2: int) = 
	let
		fun iter (i,acc) = 
			if i>day2 then acc
			else
				iter(i+1,acc@[what_month(i)])
	in
		iter(day1,[])
	end

(*ex 11*)
fun oldest(dates: (int*int*int) list) : (int*int*int) option = 
	let 
		fun find_oldest(ds: (int*int*int) list, oldest: (int*int*int)) = 
			if null ds then
				oldest 
			else if is_older((hd ds), oldest) then
				find_oldest((tl ds),(hd ds))
			else
				find_oldest((tl ds), oldest)
	in
		if null dates then NONE
		else
			SOME(find_oldest((tl dates), (hd dates)))
	end 	

(*ex 12*)
fun remove_duplicates(intList : int list) : int list =
	let 
		fun contains(ilist: int list, element: int) = 
			if null ilist then false
			else if (hd ilist)=element then true
			else contains(tl ilist,element)
			
		fun remove_duplicated(ilist: int list, resultList: int list) =
			if null ilist then 
				resultList
			else if contains(resultList, hd ilist) then 
				remove_duplicated((tl ilist),resultList)
			else 
				remove_duplicated((tl ilist),resultList@((hd ilist)::[]))
	in
		remove_duplicated(intList,[])
	end
	
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) : int =
	number_in_months(dates, remove_duplicates(months))
	
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) : (int*int*int) list =
	dates_in_months(dates,remove_duplicates(months))
	

(*ex 13*)
fun is_leap_year(year: int) : bool = 
	year mod 400 = 0  orelse (year mod 4 = 0 andalso not (year mod 100 = 0))

fun number_of_days_in_month(year: int, month: int) =
	let
		fun get_nth (intList: int list, n: int,i : int) =
			if i=n then hd intList
			else get_nth(tl intList,n,i+1)
		val ndays_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	in 
		if (is_leap_year(year)) andalso month = 2 then 29
		else get_nth(ndays_in_month, month,1)
	end
	
fun reasonable_date(date : (int*int*int)) : bool =
	if (#1 date < 1) orelse (#2 date<1) orelse (#2 date>12) orelse (#3 date<1) orelse (#3 date> 31)  then false
	else if #3 date <= number_of_days_in_month(#1 date, #2 date) then true
	else false 