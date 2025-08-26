(* Based on Rust implementation in this repo (bench/algorithm/secp256k1/1.rs) and TypeScript 1.ts; uses Zarith (Z) for big integers. *)

module Z = Z

(* Curve constants for secp256k1 *)
module Curve = struct
  let p =
    Z.(sub (sub (pow (of_int 2) 256) (pow (of_int 2) 32)) (of_int 977))

  let n =
    Z.(sub (pow (of_int 2) 256)
         (of_string
            "432420386565659656852420866394968145599"))

  let gx =
    Z.of_string
      "55066263022277343669578718895168534326250603453777594175500187360389116729240"

  let gy =
    Z.of_string
      "32670510020758816978083085130507043184471273380659243275938904335757337482424"

  let beta =
    (* 0x7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee *)
    Z.of_string
      "0x7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee"
end

let pow2_128 = Z.pow (Z.of_int 2) 128

(* Private key used in tests/benchmarks (decimal) *)
let private_key =
  Z.of_string
    "20775598474904240222758871485654738649026525153462921990999819694398496339603"

(* Endomorphism decomposition constants *)
let a1 = Z.of_string "0x3086d221a7d46bcde86c90e49284eb15"
let b1 = Z.of_string "-0xe4437ed6010e88286f547fa90abfe4c3"
let a2 = Z.of_string "0x114ca50f7a8e2f3f657c1108d9d44cfd8"
let b2 = a1

let rem_p a =
  (* Euclidean remainder ensures 0 <= r < p *)
  Z.erem a Curve.p

let rem_n a = Z.erem a Curve.n

let invert number =
  (* Extended Euclidean Algorithm modulo p *)
  let modulo = Curve.p in
  let a = ref (rem_p number) in
  let b = ref modulo in
  let x = ref Z.zero in
  let y = ref Z.one in
  let u = ref Z.one in
  let v = ref Z.zero in
  while not (Z.equal !a Z.zero) do
    let q = Z.div !b !a in
    let r = Z.sub !b (Z.mul q !a) in
    let m = Z.sub !x (Z.mul !u q) in
    let n = Z.sub !y (Z.mul !v q) in
    b := !a;
    a := r;
    x := !u;
    y := !v;
    u := m;
    v := n
  done;
  rem_p !x

let div_nearest a b = Z.div (Z.add a (Z.div b (Z.of_int 2))) b

let split_scalar_endo k =
  let n = Curve.n in
  let c1 = div_nearest (Z.mul b2 k) n in
  let c2 = div_nearest (Z.neg (Z.mul b1 k)) n in
  let k1 = rem_n (Z.sub (Z.sub k (Z.mul c1 a1)) (Z.mul c2 a2)) in
  let k2 = rem_n (Z.sub (Z.neg (Z.mul c1 b1)) (Z.mul c2 b2)) in
  let k1neg = Z.compare k1 pow2_128 > 0 in
  let k2neg = Z.compare k2 pow2_128 > 0 in
  let k1 = if k1neg then Z.sub n k1 else k1 in
  let k2 = if k2neg then Z.sub n k2 else k2 in
  (k1neg, k1, k2neg, k2)

type jacobian = { x : Z.t; y : Z.t; z : Z.t }
type point = { ax : Z.t; ay : Z.t }

let jacobian_zero = { x = Z.zero; y = Z.one; z = Z.zero }

let point_generator = { ax = Curve.gx; ay = Curve.gy }

let jacobian_of_affine (p : point) = { x = p.ax; y = p.ay; z = Z.one }

let affine_of_jacobian (p : jacobian) : point =
  let inv_z = invert p.z in
  let inv_z2 = rem_p (Z.mul inv_z inv_z) in
  let x = rem_p (Z.mul p.x inv_z2) in
  let y = rem_p (Z.mul p.y (rem_p (Z.mul inv_z inv_z2))) in
  { ax = x; ay = y }

let jacobian_negate (p : jacobian) = { x = p.x; y = rem_p (Z.neg p.y); z = p.z }

let jacobian_double (p : jacobian) : jacobian =
  (* a,b,c,d,e,f as in reference *)
  let a = rem_p (Z.mul p.x p.x) in
  let b = rem_p (Z.mul p.y p.y) in
  let c = rem_p (Z.mul b b) in
  let x_plus_b = Z.add p.x b in
  let xpb2 = rem_p (Z.mul x_plus_b x_plus_b) in
  let d = rem_p (Z.mul (Z.of_int 2) (rem_p (Z.sub (rem_p (Z.sub xpb2 a)) c))) in
  let e = rem_p (Z.mul (Z.of_int 3) a) in
  let f = rem_p (Z.mul e e) in
  let x3 = rem_p (Z.sub f (Z.mul (Z.of_int 2) d)) in
  let y3 = rem_p (Z.sub (Z.mul e (Z.sub d x3)) (Z.mul (Z.of_int 8) c)) in
  let z3 = rem_p (Z.mul (Z.of_int 2) (rem_p (Z.mul p.y p.z))) in
  { x = x3; y = y3; z = z3 }

let jacobian_add (p : jacobian) (q : jacobian) : jacobian =
  if Z.equal q.x Z.zero || Z.equal q.y Z.zero then p
  else if Z.equal p.x Z.zero || Z.equal p.y Z.zero then q
  else
    let z1z1 = rem_p (Z.mul p.z p.z) in
    let z2z2 = rem_p (Z.mul q.z q.z) in
    let u1 = rem_p (Z.mul p.x z2z2) in
    let u2 = rem_p (Z.mul q.x z1z1) in
    let s1 = rem_p (Z.mul p.y (rem_p (Z.mul q.z z2z2))) in
    let s2 = rem_p (Z.mul (rem_p (Z.mul q.y p.z)) z1z1) in
    let h = rem_p (Z.sub u2 u1) in
    let r = rem_p (Z.sub s2 s1) in
    if Z.equal h Z.zero then if Z.equal r Z.zero then jacobian_double p else jacobian_zero
    else
      let hh = rem_p (Z.mul h h) in
      let hhh = rem_p (Z.mul h hh) in
      let v = rem_p (Z.mul u1 hh) in
      let x3 = rem_p (Z.sub (Z.sub (rem_p (Z.mul r r)) hhh) (Z.mul (Z.of_int 2) v)) in
      let y3 = rem_p (Z.sub (Z.mul r (Z.sub v x3)) (rem_p (Z.mul s1 hhh))) in
      let z3 = rem_p (Z.mul (rem_p (Z.mul p.z q.z)) h) in
      { x = x3; y = y3; z = z3 }

let jacobian_mul_unsafe (p : jacobian) (scalar : Z.t) : jacobian =
  let k1neg, k1, k2neg, k2 = split_scalar_endo scalar in
  let rec loop acc1 acc2 d k1 k2 =
    let acc1 = if Z.testbit k1 0 then jacobian_add acc1 d else acc1 in
    let acc2 = if Z.testbit k2 0 then jacobian_add acc2 d else acc2 in
    let d2 = jacobian_double d in
    let k1' = Z.shift_right k1 1 in
    let k2' = Z.shift_right k2 1 in
    if Z.equal k1 Z.zero && Z.equal k2 Z.zero then (acc1, acc2)
    else loop acc1 acc2 d2 k1' k2'
  in
  let k1p, k2p = loop jacobian_zero jacobian_zero p k1 k2 in
  let k1p = if k1neg then jacobian_negate k1p else k1p in
  let k2p = if k2neg then jacobian_negate k2p else k2p in
  let k2p = { k2p with x = rem_p (Z.mul k2p.x Curve.beta) } in
  jacobian_add k1p k2p

let point_mul (p : point) (scalar : Z.t) : point =
  affine_of_jacobian (jacobian_mul_unsafe (jacobian_of_affine p) scalar)

let rec to_hex_rev acc z =
  if Z.equal z Z.zero then acc
  else
    let q, r = Z.ediv_rem z (Z.of_int 16) in
    let d = Z.to_int r in
    let c =
      if d < 10 then Char.chr (Char.code '0' + d)
      else Char.chr (Char.code 'a' + (d - 10))
    in
    to_hex_rev (c :: acc) q

let z_to_hex z =
  if Z.equal z Z.zero then "0"
  else
    let chars = to_hex_rev [] z in
    String.init (List.length chars) (List.nth chars)

let () =
  let n =
    try int_of_string (Array.get Sys.argv 1) with _ -> 1
  in
  let rec iter i pt = if i = 0 then pt else iter (i - 1) (point_mul pt private_key) in
  let point = iter n point_generator in
  Printf.printf "%s,%s\n" (z_to_hex point.ax) (z_to_hex point.ay)
