(** Generator for rubik's group *)

module Vec = struct
  type pos = {x:int; y: int; z: int}

  let map ( % ) a b = { x = a.x % b.x;
                        y = a.y % b.y;
                        z = a.z % b.z;
                      }
  let (|*|) a b  =  a.x * b.x
                  + a.y * b.y
                  + a.z * b.z
  let (+)= map (+)
  let (-) = map (-)
  let ( * ) s a = { x = s * a.x;
                    y = s * a.y;
                    z = s * a.z;
                  }
  let pp ppf {x;y;z} = Format.fprintf ppf "(%2d %2d %2d)" x y z
end

(*
-2 -1 0 -1 2 
*)

let vec x y z = { Vec.x; y ;z }

type color = R|G|Y|B|W|O

let face c corner x y =
  let open Vec in
  List.map (fun x -> c, x )
    [
      corner         ; corner + x         ; corner+  2 * x;
      corner + y     ; corner + x + y     ; corner + 2 * x + y;
      corner + 2 * y ; corner + x + 2 * y ; corner + 2 * ( x + y )
    ]

let x = vec 1 0 0
let y = vec 0 1 0
let z = vec 0 0 1

let basis = [x; y ; z ]

let except v =
  match List.filter ( (<>) v ) basis with
  | [x;y] -> x, y
  | _ -> assert false

let corner = Vec.(vec 0 0 0 - vec 1 1 1)

let face' c c' z =
  let x,y = except z in
  [face c Vec.(corner - z ) x y;
   face c' Vec.(corner + 3 * z) x y]

let pp_pair ppf (x,y) = Format.fprintf ppf "%d => %d" x y

let pp_list elt =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ") elt


let cube =
  Array.of_list
  @@ List.flatten
  @@ (face' R B z @ face' G Y x @ face' W O y)


;;

let pp_point ppf (_,_,x) = Format.fprintf ppf "%a" Vec.pp x

let rec pp_points ppf = function
  | x :: y :: z ::l ->
    Format.fprintf ppf "%a %a %a@,%a" pp_point x pp_point y pp_point z
      pp_points l
  | l -> pp_list pp_point ppf l


let lift_t f (c,x) = (c, f x)
let size = Array.length cube


let index_of (c,x) cube' =
  let exception Found of int in
  try
    Array.iteri ((fun i (_,x') -> if x = x' then raise (Found i))) cube';
    assert false
  with
    Found i -> i

let mapping f =
  let cube' = Array.map (lift_t f) cube in
(*  Format.printf "@[<v>%a@]@." pp_points cube;
    Format.printf "@[<v>%a@]@." pp_points cube';*)
  Array.init size (fun n -> index_of cube.(n) cube')


let xy v = Vec.{ v with x = -v.y; y = v.x }
let xz v = Vec.{ v with x = -v.z; z = v.x }
let yz v = Vec.{ v with y = -v.z; z = v.y }

let rotate z =
  let x,y = except z in
  let open Vec in
  (fun v -> (x|*|v) * y - (y|*|v) * x + (v|*|z) * z),
  (fun v -> (y|*|v) * x - (x|*|v) * y + (x|*|z) * z)

let (&&&) f g v = if f v then g v else v

let plus v x = Vec.(x|*|v) >= 1
let minus v x = Vec.(v|*|x) <= -1
let zero v x = Vec.(v|*|x) = 0

let transf v =
  let r, r' = rotate v in
  [ plus v &&& r; zero v &&& r ; minus v &&& r;
    plus v &&& r'; zero v &&& r' ; minus v &&& r' ]

let transfs =
  List.flatten @@ List.map transf basis

let xy_plus = plus z &&& fst @@ rotate z

let print_f f =
  let l = mapping f in
  Format.printf "@[%a@]@." (pp_list pp_pair)
    (Array.to_list @@ Array.mapi (fun i n -> i, n) l)

