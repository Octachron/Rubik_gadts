open Action

type 'a t =
  | [] : init t
  | (::): ('a,'b) action  * 'a t -> 'b t

let x: init t = [Xpp;Xpp;Xpp;Xpp]


module type t = sig type t end
let lift (type t) (x:t): (module t with type t = t ) =
  (module struct type nonrec t = t end)

module Point = (val lift [Xpp;Ypp;Xpp])

type truth = T
let diameter_is_greater_than_three: Point.t -> truth =
  function
  |  _::_::_::_ -> T
  | _ -> .
