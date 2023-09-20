#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

llet isPrime n =
  let is_divisible x = n mod x = 0 in
  let test i =
    if i <= 1 then false
    else not (int1_forall (i - 1) is_divisible)
  in
  if n < 2 then false
  else int1_forall n test

(* ************************************************ *)
