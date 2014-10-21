use "student2-e510b2b04b9311e497cb3fa30953733f.sml";

(* 1 *)

val date1 = (2014, 01, 30);
val date2 = (2014, 02, 20);

is_older(date1, date2) = true;

val date1 = (2013, 01, 30);
val date2 = (2014, 01, 30);

is_older(date1, date2) = true;

val date1 = (2013, 05, 30);
val date2 = (2013, 04, 31);

is_older(date1, date2) = false;
is_older(date1, date1) = false;

val date1 = (2014, 05, 30);
val date2 = (2013, 05, 30);

is_older(date1, date2) = false;

val date1 = (2014, 05, 30);
val date2 = (2013, 05, 30);

is_older(date1, date2) = false;


(* 2 *)

val dates = [
	(2014, 01, 01),
	(2014, 02, 21),
	(2014, 03, 01),
	(2014, 99, 100),
	(2014, 03, 01),
	(2014, 06, 01),
	(2014, 12, 01),
	(2014, 05, 01),
	(2014, 02, 22),
	(2013, 12, 31),
	(2014, 99, 9001),
	(2014, 01, 02)
];

number_in_month(dates, 2) = 2;
number_in_month(dates, 1) = 2;
number_in_month(dates, 3) = 2;
number_in_month(dates, 99) = 2;
number_in_month(dates, 6) = 1;


(* 3 *)

val months = [3, 5, 99, 2];

number_in_months(dates, months) = 7;

(* 4 *)

dates_in_month(dates, 1) = [(2014,1,1),(2014,1,2)];
dates_in_month(dates, 2) = [(2014,2,21),(2014,2,22)];
dates_in_month(dates, 20) = [];
dates_in_month(dates, 99) = [(2014, 99, 100), (2014, 99, 9001)];

(* 5 *)
dates_in_months(dates, months) = [
	(2014, 03, 01),
	(2014, 03, 01),
	(2014, 05, 01),
	(2014, 99, 100),
	(2014, 99, 9001),
	(2014, 02, 21),
	(2014, 02, 22)
];
dates_in_months([], []) = [];

(* 6 *)
val strings = [
	"a",
	"b",
	"c",
	"d",
	"e"
];

get_nth(strings, 1) = "a";
get_nth(strings, 2) = "b";
get_nth(strings, 6) = "";

(* 7 *)
date_to_string((2014, 01, 01)) = "January 1, 2014";
date_to_string((2000, 10, 25)) = "October 25, 2000";

(* 8 *)
val numbers = [1,2,3,4,5];
number_before_reaching_sum(4, numbers) = 2;
number_before_reaching_sum(5, numbers) = 2;
number_before_reaching_sum(7, numbers) = 3;
number_before_reaching_sum(8, numbers) = 3;
number_before_reaching_sum(1, numbers) = 0;
(* number_before_reaching_sum(100, numbers); infinite loop *)

(* 9 *)
what_month(1) = 1;
what_month(31) = 1;
what_month(32) = 2;
what_month(365) = 12;
what_month(330) = 11;

(* 10 *)
month_range(30, 34) = [1,1,2,2,2];
month_range(32, 30) = [];
month_range(30, 30) = [1];

(* 11 *)
oldest([]) = NONE;
oldest(dates) = SOME((2013, 12, 31));

(* 12 *)
val months_dups = [3, 5, 99, 99, 2, 5, 3];

number_in_months_challenge(dates, months_dups) = 7;
dates_in_months_challenge(dates, months_dups);
dates_in_months_challenge(dates, months_dups) = [
	(2014, 99, 100),
	(2014, 99, 9001),
	(2014, 02, 21),
	(2014, 02, 22),
	(2014, 05, 01),
	(2014, 03, 01),
	(2014, 03, 01)
];
dates_in_months_challenge([], []) = [];

(* 13 *)
reasonable_date((2013, 01, 01)) = true;
reasonable_date((2004, 02, 28)) = true;
reasonable_date((2004, 02, 29)) = true;
reasonable_date((2004, 02, 30)) = false;
reasonable_date((2003, 03, 30)) = true;
reasonable_date((10000, 12, 12)) = true;
reasonable_date((2004, 13, 30)) = false;
reasonable_date((2001, 10, 32)) = false;
reasonable_date((0, 06, 30)) = false;
reasonable_date((~1, 02, 1)) = false;