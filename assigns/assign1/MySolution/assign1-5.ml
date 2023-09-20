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
let string_length= String.length;;
let string_init = String.init;;


let string_longest_ascend(xs: string): string =
  let len = string_length xs in
  let rec find_longest_sequence start end_pos max_start max_len =
    if end_pos = len then
      let longest_subseq = string_init max_len (fun i -> string_get_at xs (max_start + i)) in
      longest_subseq
    else
      let c1 = string_get_at xs start in
      let c2 = string_get_at xs end_pos in
      if c1 <= c2 then
        find_longest_sequence start (end_pos + 1) max_start (max_len + 1)
      else
        let new_start = end_pos in
        if max_len > (new_start - max_start) then
          find_longest_sequence new_start (new_start + 1) new_start max_len
        else
          find_longest_sequence new_start (new_start + 1) max_start (new_start - max_start + 1)
  in
  if len = 0 then
    ""
  else
    find_longest_sequence 0 1 0 1
;;
;;

(* Example usage *)

