(* Coursera Programming Languages, Homework 3, Provided Code *)
fun only_capitals (slist) =
   List.filter  (fn s => Char.isUpper( String.sub(s, 0) )) slist


fun longest_string1 slist = 
   let fun  combine (x, y) = 
       if String.size x > String.size y 
       then x
       else y
   in List.foldl combine "" slist end

fun longest_string2 slist = 
  let fun combine(x, y) = 
      if String.size x >= String.size y
      then x
      else y
  in 
     List.foldl combine "" slist
  end
(* problems 4 *)
fun longest_string_helper ( f ) = 
    if f (1,  1) 
    then longest_string2
    else longest_string1

val longest_string3 = longest_string_helper  ( fn (x, y) => x > y ) 
val longest_string4 = longest_string_helper  ( fn (x, y) => x >= y )

(* problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* problem 6 *)
val rev_string = String.implode o List.rev o String.explode


exception NoAnswer
(* problem 7 *)
fun first_answer f alist = 
      case alist of 
      [] => raise NoAnswer
     | x::xs' =>   case f x of
                  SOME y => y
	         |NONE  => first_answer f xs'
    
(* problems 8  *)		    
fun all_answers f = 
    let fun helper ([], acc ) = acc
          | helper (x::xs', acc)  = case acc of
                  NONE => NONE
		 |SOME y  => case f x of 
		            NONE => NONE
			  | SOME z  => helper(xs', SOME (y  @ z ))
    in 
       fn alist => helper (alist, SOME[] )
    end


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


val count_wildcards =
       g (fn () => 1) ( fn z => 0 ) 
    

val count_wild_and_variable_lengths =
       g (fn () => 1) (fn z => String.size z )

val count_some_var = fn  (str, pattern ) =>
    g (fn() => 0) (fn z => if z = str then 1 else 0 ) pattern

(* problem 10 *)
fun check_pat pattern = 
    let
       fun all_names p = 
           case p of 
           Wildcard => []
         | Variable v => [v]
         | TupleP tp  => List.foldl  (fn (p, xs') => (all_names p) @ xs') [] tp
         | ConstructorP (_, p)  => all_names p
         | _  => []
      fun nodup [] = true
	| nodup (x::xs) =  if ( List.exists (fn x' => x = x' ) xs) then false 
                       else nodup xs
    in
      nodup ( all_names pattern )
    end
             
(* problem 11 *)
fun match vp =
   case vp of
     (_, Wildcard) => SOME []
   | ( v', Variable s) => SOME [(s, v')]
   | (Unit, UnitP)     => SOME []
   | (Const i, ConstP ip)  => if i= ip then SOME [] else NONE
   | (Tuple vlist, TupleP plist)  => 
                if ( List.length vlist ) = ( List.length plist ) 
                then  all_answers match ( ListPair.zip (vlist, plist ))
                else NONE
   | (Constructor (vs,v), ConstructorP(ps, p) ) =>
                if vs = ps then match(v, p)  else NONE
   | (_, _) => NONE    

(* problem 12 *)

fun first_match value =
    fn plist => SOME ( first_answer ( fn p => match (value,p) ) plist )
                handle NoAnswer => NONE     

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
