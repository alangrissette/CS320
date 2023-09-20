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

let string_get_at(cs:string)(i0:int): char = String.get cs i0;;
let string_length = String.length;;


let string_avoid_132(cs: string): bool =
  let len = string_length cs in
  if len < 3 then true (* Strings with fewer than 3 characters are 132-avoid by definition. *)
  else
    let rec find_132_like i min_char max_char =
      if i >= len then true
      else
        let current_char = string_get_at cs i in
        if current_char >= max_char then
          find_132_like (i + 1) min_char current_char
        else if current_char > min_char then
          false (* Found a 132-like sequence *)
        else
          find_132_like (i + 1) min_char max_char
    in
    find_132_like 1 (string_get_at cs 0) (string_get_at cs 0)
;;