(* 1 *)
fun is_older( d1: int*int*int, d2:int*int*int ) =
    if (#1 d1) < (#1 d2 )
    then true
    else
    if (#1 d1 ) = (#1 d2 ) andalso (#2 d1) < (#2 d2)
    then true
    else
    if (#2 d1 ) = (#2 d2) andalso (#3 d1) < (#3 d2)
    then true
    else false
(* 2 *)
fun number_in_month ( dates:(int*int*int) list, month:int )=
    if null dates
    then 0
    else
      let val cnt = number_in_month( tl dates, month )
      in
        if (#2 (hd dates )) = month 
        then 1 + cnt
        else cnt
      end
(* 3 *)
fun number_in_months( dates: (int*int*int) list, months : int list ) = 
  if null months
  then 0
  else number_in_month( dates, hd months)+  number_in_months( dates, tl months)

(* 4 *)
fun dates_in_month( dates:(int*int*int) list, month: int ) = 
  if null dates
  then []
  else
    let val tl_dates = dates_in_month( tl dates, month)
    in
      if #2 (hd dates ) = month
      then (hd dates) ::tl_dates
      else tl_dates
    end

(* 5 *)
fun dates_in_months( dates:(int*int*int) list, months: int list ) = 
  if null months
  then []
  else dates_in_month( dates, hd months) @ dates_in_months(dates, tl months )

(* 6 *)
fun get_nth( slist: string list, n: int ) = 
  let 
    fun find_nth(s:string list, i:int) = 
      if i = n
      then hd s
      else find_nth(tl s, i+1)
  in
    find_nth(slist, 1)
  end

(* 7 *)
fun date_to_string( d: int*int*int ) = 
  let
    val month_list = ["January","February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December"]
  in
    get_nth(month_list, (#2 d)) ^ " " ^ Int.toString(#3 d)
    ^ ", " ^ Int.toString(#1 d)
  end

(* 8 *)
fun number_before_reaching_sum(sum: int,  nums : int list ) = 
  let 
    fun find_n( sub_nums: int list, hd_sum: int,  pos:int ) =
      if  hd_sum >= sum 
      then pos - 1
      else find_n(tl sub_nums, hd_sum + (hd sub_nums), pos + 1)
  in
    find_n( nums, 0, 0 )
  end
 
(* 9 *)
fun what_month (days: int ) = 
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(days, days_in_month) + 1
  end
    
(* 10 *)
fun month_range( day1:int, day2:int ) = 
  if day1 > day2
  then []
  else what_month( day1 ) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest( dates : (int*int*int) list ) = 
  if null dates
  then NONE
  else 
    let val oldest_tl = oldest( tl dates )
      in 
      if isSome oldest_tl andalso is_older(valOf oldest_tl, hd dates)
      then oldest_tl
      else SOME (hd dates)
    end

(* 12 *)
fun remove_dup ( nums : int list ) =
let 
  fun exist (x:int, sub_res: int list ) =
    if null sub_res
    then false
    else 
      if hd sub_res = x
      then true
      else exist(x, tl sub_res)

  fun traverse (sub_nums  : int list ) = 
    if null sub_nums
    then []
    else
      let val res = traverse( tl sub_nums )
      in 
        if exist( hd sub_nums, res )
        then res
        else (hd sub_nums) :: res
      end
in 
  traverse( nums )
end
   
fun number_in_months_challenge ( dates: (int*int*int) list, months: int list) =
  number_in_months( dates, remove_dup( months ) )

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) = 
  dates_in_months( dates, remove_dup( months) )

(* 13 *)

fun reasonable_date( date: int*int*int ) =
  if (#1 date) <= 0
  then false
  else
    if(#2 date) <=0 orelse (#2 date) > 12
    then false
    else
      let 
        val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31];
        fun get_days ( n : int, months: int list ) =
          if n = 1
          then hd months
          else get_days(n-1, tl months)

        fun add_leap_day ( year: int, month:int) = 
          if month =2 andalso
	     (  year mod 400 = 0 orelse ( year mod 100 <> 0  andalso year mod 4 = 0 ))
          then 1
          else 0
      in
      if #3 date > 0 
         andalso  #3 date <=  get_days(#2 date, days_in_months) + add_leap_day(#1 date, #2 date)
      then true
      else false
      end
          

  
