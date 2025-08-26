
(* Based on Rust implementation in this repo (bench/algorithm/pidigits/1.rs); uses Zarith (Z) for big integers. *)
(* Zarith big integers *)
module Z = Z

let () =
  (* number of digits to output, default 27 *)
  let digits_to_print =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 27
  in

  let digits_printed = ref 0 in

  (* Spigot state, all bigints *)
  let k = ref (Z.of_int 1) in
  let n1 = ref (Z.of_int 4) in
  let n2 = ref (Z.of_int 3) in
  let d = ref (Z.of_int 1) in

  let rec loop () =
    (* u = n1 / d; v = n2 / d *)
  let u = Z.div !n1 !d in
  let v = Z.div !n2 !d in
  if Z.equal u v then (
      (* emit digit u *)
  print_int (Z.to_int u);
      incr digits_printed;
  if (!digits_printed) mod 10 = 0 then
        Printf.printf "\t:%d\n" !digits_printed;

      if !digits_printed >= digits_to_print then (
        let rem = (!digits_printed) mod 10 in
        if rem <> 0 then (
          for _i = rem to 9 do print_char ' ' done;
          Printf.printf "\t:%d\n" !digits_printed
        );
        flush stdout
      ) else (
        let to_minus = Z.mul (Z.mul u (Z.of_int 10)) !d in
        n1 := Z.sub (Z.mul !n1 (Z.of_int 10)) to_minus;
        n2 := Z.sub (Z.mul !n2 (Z.of_int 10)) to_minus;
        loop ()
      )
    ) else (
      (* refine *)
      let k2 = Z.mul !k (Z.of_int 2) in
      let u' = Z.mul !n1 (Z.sub k2 (Z.of_int 1)) in
      let v' = Z.mul !n2 (Z.of_int 2) in
      let w' = Z.mul !n1 (Z.sub !k (Z.of_int 1)) in
      n1 := Z.add u' v';
      let u'' = Z.mul !n2 (Z.add !k (Z.of_int 2)) in
      n2 := Z.add w' u'';
      d := Z.mul !d (Z.add k2 (Z.of_int 1));
      k := Z.add !k (Z.of_int 1);
      loop ()
    )
  in
  loop ()
