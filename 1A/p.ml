
let flagstones n m a =
  ceil(n /. a) *. ceil(m /. a)

let () =
  let f x y z = (x, y, z) in
  let (n, m, a) = Scanf.bscanf Scanf.Scanning.stdin "%f %f %f" f in
  Printf.printf "%.0f\n" (flagstones n m a)

