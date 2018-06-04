
(** The module defines the rubik group
    and the initial state of the cube *)
open Rubik_group

(** ['a seq] represents a sequence of moves that
    result in the state ['a] *)
type 'a seq =
  | [] : init seq
  | (::): ('a,'b) move  * 'a seq -> 'b seq

(** Example: Rotating the superior part of the x plane in the
    direct sense four times, yields the initial state of the cube
*)
let rotate_four: 'a seq -> 'a seq =
  fun x -> Xpp::Xpp::Xpp::Xpp::x

(** Similarly, rotating in one direction and then in the other
    does not change the state of the cube *)
let rec rotate_back_and_forth: 'a seq -> 'a seq =
  fun x -> Xpp::Xpm::x


(** We want to lift the result type of a sequence to the module
    level, to be able to speak about it in function types. *)
module type t = sig type t end
let lift (type t) (x:t): (module t with type t = t ) =
  (module struct type nonrec t = t end)

(** [Point.t] is the resulting state of the cube after the
    following sequence of actions *)
module Point = (val lift [Xpp;Ypp;Xpp;Zpp; Ypm; Xpm; Zmm])

(** We are not really interested to what happens at the type
    level, that's why we return truth *)
type truth = T

(** We now ask the typechecker to check that is not possible
    to reach the state [Point.t] in less than 4 move *)
let diameter_is_greater_than_four: Point.t -> truth =
  function
  | _ :: _ :: _ :: _ :: _ ->
    (*We are not interested in sequence of more than 4 moves *)
    T
  | _ -> .
    (* ↑ This is the statement of the theorem than the diameter
         of the Rubik's cube group is greater than 4 *)
