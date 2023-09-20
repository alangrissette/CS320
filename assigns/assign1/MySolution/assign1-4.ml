let rec list_revapp(xs: 'a list)(ys: 'a list): 'a list =
  match xs with
  | [] -> ys
  | x1 :: xs -> list_revapp(xs)(x1 :: ys)
;;
let list_reverse(xs: 'a list): 'a list = list_revapp(xs)([]);;

let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res))
    in(*let*)(fwork(work); list_reverse(!res) )
;;
let string_make_fwork(fwork: (char -> unit) -> unit): string =
  let xs =
    Array.of_list(list_make_fwork(fwork)) 
  in String.init (Array.length(xs)) (fun i -> xs.(i))
;;
let ord = Char.code;;
let chr = Char.chr;;

let char_of_digit (d0: int): char =
  let () = assert(d0 >= 0) in
    let () = assert(d0 <= 9) in 
      chr(ord('0') + d0)
;;(* end of [char_of_digit] *)

let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in 
      ord(ch) - ord('0')
;;(* end of [digit_of_char] *)

let rec list_foreach(xs: 'a list) (work: 'a -> unit): unit =
  (
    match xs with
    | [] -> ()
    | x1 :: xs -> (
        work(x1); list_foreach(xs)(work)
      )
  )
;;

let string_get_at(cs:string)(i0:int): char = String.get cs i0;;
let string_length = String.length;;


let intrep_add(ds1: string)(ds2: string): string =
  let len1 = string_length ds1 in
  let len2 = string_length ds2 in
  let max_len = max len1 len2 in

  let digit_at s i =
    if i < string_length s then
      match string_get_at s i with
      | '0' .. '9' as c -> digit_of_char c
      | _ -> 0 (* Ignore non-digit characters *)
    else
      0
  in

  let rec add_digits i carry result =
    if i < max_len then
      let sum = digit_at ds1 i + digit_at ds2 i + carry in
      let new_carry = sum / 10 in
      let digit = char_of_digit (sum mod 10) in
      add_digits (i + 1) new_carry (digit :: result)
    else
      match carry with
      | 0 -> result
      | _ -> '1' :: result
  in

  let result_chars = add_digits 0 0 [] in

  string_make_fwork (fun work -> list_foreach (list_reverse result_chars) work)
;;

