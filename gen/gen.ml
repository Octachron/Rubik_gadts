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
    Format.eprintf "Cannot find %a@." Vec.pp x;
    raise Not_found
  with
    Found i -> i

let mapping f =
  let cube' = Array.map (lift_t f) cube in
  try Array.init size (fun n -> index_of cube.(n) cube') with
  | Not_found ->
    let conv a = Array.to_list @@ Array.mapi (fun i (c,x) -> (i,c,x)) a in
    Format.printf "@[<v>%a@]@." pp_points (conv cube) ;
    Format.printf "@[<v>%a@]@." pp_points (conv cube');
    assert false



let xy v = Vec.{ v with x = -v.y; y = v.x }
let xz v = Vec.{ v with x = -v.z; z = v.x }
let yz v = Vec.{ v with y = -v.z; z = v.y }

let rotate z =
  let x,y = except z in
  let open Vec in
  (fun v -> (x|*|v) * y - (y|*|v) * x + (v|*|z) * z),
  (fun v -> (y|*|v) * x - (x|*|v) * y + (v|*|z) * z)

let (&&&) f g v = if f v then g v else v

let plus v x = Vec.(x|*|v) >= 1
let minus v x = Vec.(v|*|x) <= -1
let zero v x = Vec.(v|*|x) = 0

let transf (name, v) =
  let r, r' = rotate v in
  [ name^"pp", plus v &&& r; name ^ "mp", minus v &&& r;
     name ^ "pm", plus v &&& r';  name ^ "mm", minus v &&& r' ]

let transfs =
  List.flatten @@ List.map transf [ "X" , x; "Y", y; "Z", z]

let xy_plus = plus z &&& fst @@ rotate z

let print_f f =
  let l = mapping f in
  Format.printf "@[%a@]@." (pp_list pp_pair)
    (Array.to_list @@ Array.mapi (fun i n -> i, n) l)

let typ x y ppf = Format.fprintf ppf "type %t = %t" x y
let str x ppf = Format.fprintf ppf "%s" x
let nl ppf = Format.fprintf ppf "@,"

let start ppf =
  let colors = [ "r"; "g"; "b"; "w"; "o"; "y"] in
  Format.fprintf ppf "@[<v>";
  List.iter (fun x -> typ (str x) (str ("C" ^ x)) ppf; nl ppf ) colors;
  Format.fprintf ppf "type init = <@,";
  let counter = ref 1 in
  let print_face c =
    for i = 1 to 3 do
      for j = 1 to 3 do
        Format.fprintf ppf "f%d:%s; " !counter c; incr counter
      done;
      nl ppf;
    done; nl ppf; in
  List.iter print_face colors;
  Format.fprintf ppf ">@,@]"

let transform (name,f) ppf =
  Format.fprintf ppf "@ | %s:(@,<@[" name;
  for i = 1 to size do
    Format.fprintf ppf "f%d:'f%d;@ " i i
  done;
  Format.fprintf ppf "@]>,@ <@[";
  let m = mapping f in
  for i = 1 to size do
    Format.fprintf ppf "f%d:'f%d;@ " i (1+m.(i-1))
  done;
  Format.fprintf ppf "@]>) action"

let action ppf =
  Format.fprintf ppf "@[<v>type (_,_) action =";
  List.iter (fun x -> transform x ppf) transfs;
  Format.fprintf ppf "@,@]"

let output = ref Format.std_formatter
let set_output s = output := Format.formatter_of_out_channel (open_out s)
let args = ["-o", Arg.String set_output, "output file"]

;; Arg.parse args ignore "gen -o file"

;; Format.fprintf !output "%t%t@." start action
