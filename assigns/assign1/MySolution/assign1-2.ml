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

let string_merge(cs1: string)(cs2: string): string =
  string_make_fwork (fun work ->

    let rec merge_strings i1 i2 =
      if i1 < string_length cs1 && i2 < string_length cs2 then
        let char1 = string_get_at cs1 i1 in
        let char2 = string_get_at cs2 i2 in
        if char1 <= char2 then (
          work char1;
          merge_strings (i1 + 1) i2
        ) else (
          work char2;
          merge_strings i1 (i2 + 1)
        )
      else if i1 < string_length cs1 then (
        let char1 = string_get_at cs1 i1 in
        work char1;
        merge_strings (i1 + 1) i2
      ) else if i2 < string_length cs2 then (
        let char2 = string_get_at cs2 i2 in
        work char2;
        merge_strings i1 (i2 + 1)
      )
    in
    merge_strings 0 0
  )
;;