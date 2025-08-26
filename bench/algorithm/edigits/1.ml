(* Based on Rust implementation in this repo (bench/algorithm/edigits/1.rs); uses Zarith (Z) for big integers. *)
module Z = Z

let binary_search f =
  let rec grow a b =
    if f b then (a, b) else grow b (b * 2)
  in
  let a, b = grow 0 1 in
  let rec search a b =
    if b - a <= 1 then b
    else
      let m = a + (b - a) / 2 in
      if f m then search a m else search m b
  in
  search a b

let rec sum_terms a b : Z.t * Z.t =
  if b = a + 1 then (Z.of_int 1, Z.of_int b)
  else
    let mid = (a + b) / 2 in
    let (p_left, q_left) = sum_terms a mid in
    let (p_right, q_right) = sum_terms mid b in
    (* p/q = p_left/q_left + (a!/mid!) * p_right/q_right, and a!/mid! = 1/q_left *)
    (* => p/q = (p_left*q_right + p_right) / (q_left*q_right) *)
    (Z.add (Z.mul p_left q_right) p_right, Z.mul q_left q_right)

let calculate n =
  assert (n > 0);
  (* Find k such that log10(k!) >= n + 50 via Stirling's approximation. *)
  let ln10 = log 10. in
  let tau = 2. *. Float.pi in
  let cond k =
    k > 0 &&
    let kf = float_of_int k in
    let ln_k_fact = kf *. log kf -. kf +. 0.5 *. log (tau *. kf) in
    let log10_k_fact = ln_k_fact /. ln10 in
    log10_k_fact >= float_of_int (n + 50)
  in
  let k = binary_search cond in
  (* 1/1! + ... + 1/(k-1)! *)
  let p, q = sum_terms 0 (k - 1) in
  (* Add 1/0! = 1 => p + q over q *)
  let p = Z.add p q in
  (* answer = floor( (p/q) * 10^(n-1) ) *)
  let pow10 = Z.pow (Z.of_int 10) (n - 1) in
  let answer_int = Z.div (Z.mul p pow10) q in
  Z.to_string answer_int

let () =
  let n =
    try int_of_string Sys.argv.(1) with _ -> 27
  in
  let s = calculate n in
  (* Print in groups of 10 digits with count *)
  let len = String.length s in
  (* Defensive: if len < n, left-pad with zeros to n digits to match format. *)
  let s = if len < n then String.make (n - len) '0' ^ s else s in
  let rec loop i =
    if i >= n then ()
    else
      let count = min (i + 10) n in
      let chunk_len = count - i in
      let chunk = String.sub s i chunk_len in
      if chunk_len = 10 then
        Printf.printf "%s\t:%d\n" chunk count
      else (
        (* digits followed by spaces to fill 10-width *)
        Printf.printf "%s%s\t:%d\n" chunk (String.make (10 - chunk_len) ' ') count
      );
      loop (i + 10)
  in
  loop 0
