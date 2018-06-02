type red = private R
type blue = private B
type green = private G
type yellow = private Y
type white = private W
type orange = private O


type _ facet =
  | R: red facet
  | B: blue facet
  | G: green facet
  | O: orange facet
  | W: white facet
  | Y: yellow facet

type 'p row = private Row
  constraint 'p = 'a facet * 'b facet * 'c facet


type 'p face = private Face
  constraint 'p = 'a row * 'b row * 'c row

type 'p cube = private Cube
  constraint 'p =
  <
    f: 'r face;
    b: 'y face;
    l: 'g face;
    r: 'w face;
    u: 'o face;
    d: 'b face
   >

type 'a urow = ('a facet * 'a facet * 'a facet) row
type 'a uface = ('a urow * 'a urow * 'a urow) face
type init = < f: red uface;
        b:white uface;
        l: yellow uface;
        r: orange uface;
        u: blue uface;
        d: green uface >

module Action = struct

  type 'a f = private F
    constraint 'a =
      <f:( ('f11  * 'f12  * 'f13 ) row *
           ('f21  * 'f22  * 'f23 ) row *
           ('f31  * 'f32  * 'f33 ) row ) face;
      b:'b;
      u:('u1 row * 'u2 row * 'u3 row) face;
      r:('r1 row * 'r2 row * 'r3 row) face;
      d:('d1 row * 'd2 row * 'd3 row) face;
      l:('l1 row * 'l2 row * 'l3 row) face;
      > cube
        *
        <f:(('f21 * 'f11 * 'f12) row *
            ('f31 * 'f22 * 'f13) row *
            ('f32 * 'f33 * 'f23) row ) face;
      b:'b;
      u:('l1 row * 'u2 row * 'u3 row) face;
      r:('u1 row * 'r2 row * 'r3 row) face;
      d:('r1 row * 'd2 row * 'd3 row) face;
      l:('d1 row * 'l2 row * 'l3 row) face
        > cube

  type (_,_) t =
    | F :
        (<f:( ('f11  * 'f12  * 'f13 ) row *
              ('f21  * 'f22  * 'f23 ) row *
              ('f31  * 'f32  * 'f33 ) row ) face;
          b:'b;
          u:( ('u11 * 'u12 * 'u13) row * 'u2 row * 'u3 row) face;
          r:( ('r11  * 'r12  * 'r13 ) row *
              ('r21  * 'r22  * 'r23 ) row *
              ('r31  * 'r32  * 'r33 ) row ) face;
          d:( 'd1 row * 'd2 row * ('d31 * 'd32 * 'd33) row) face;
          l:( ('l11  * 'l12  * 'l13 ) row *
              ('l21  * 'l22  * 'l23 ) row *
              ('l31  * 'l32  * 'l33 ) row ) face>
           cube,

         <f:(('f21 * 'f11 * 'f12) row *
             ('f31 * 'f22 * 'f13) row *
             ('f32 * 'f33 * 'f23) row ) face;
          b:'b;
          u:( ('l33 * 'l23 * 'l13) row * 'u2 row * 'u3 row) face;
          r:( ('u11  * 'r12  * 'r13 ) row *
              ('u12  * 'r22  * 'r23 ) row *
              ('u13  * 'r32  * 'r33 ) row ) face;
          d:(  'd1 row * 'd2 row * ('r31 * 'r21 * 'r11) row ) face;
          l:( ('l11  * 'l12  * 'd31 ) row *
              ('l21  * 'l22  * 'd32 ) row *
              ('l31  * 'l32  * 'd33 ) row ) face>
           cube) t
    | U :
        (<f: ('f1 row * 'f2 row * 'f3 row) face;
          b: ('b1 row * 'b2 row * 'b3 row) face;
          u:  ( ('u11  * 'u12  * 'u13 ) row *
                ('u21  * 'u22  * 'u23 ) row *
                ('u31  * 'u32  * 'u33 ) row ) face;
          r: ('r1 row * 'r2 row * 'r3 row) face;
          d: 'd face;
          l:('l1 row * 'l2 row * 'l3 row) face > cube,

         <f: ('l1 row * 'f2 row * 'f3 row) face;
          b: ('r1 row * 'b2 row * 'b3 row) face;
          u: ( ('u31  * 'u12  * 'u11 ) row *
               ('u32  * 'u22  * 'u12 ) row *
               ('u33  * 'u23  * 'u13 ) row ) face;
          r: ('f1 row * 'r2 row * 'r3 row) face;
          d: 'd face;
          l:('b1 row * 'l2 row * 'l3 row) face > cube) t
    | D :
        (
          <f: ('f1 row * 'f2 row * 'f3 row) face;
           b: ('b1 row * 'b2 row * 'b3 row) face;
           d:  ( ('d11  * 'd12  * 'd13 ) row *
                 ('d21  * 'd22  * 'd23 ) row *
                 ('d31  * 'd32  * 'd33 ) row ) face;
           r: ('r1 row * 'r2 row * 'r3 row) face;
           u: 'u face;
           l:('l1 row * 'l2 row * 'l3 row) face > cube,

          <f: ('f1 row * 'f2 row * 'l3 row) face;
           b: ('b1 row * 'b2 row * 'r3 row) face;
           d: ( ('d13  * 'd23  * 'd33 ) row *
                ('d12  * 'd22  * 'd32 ) row *
                ('d11  * 'd32  * 'd31 ) row ) face;
           r: ('r1 row * 'r2 row * 'f3 row) face;
           u: 'u face;
           l: ('l1 row * 'l2 row * 'b3 row) face > cube) t
    | L:(
        <f:( ('f11  * 'f12  * 'f13 ) row *
             ('f21  * 'f22  * 'f23 ) row *
             ('f31  * 'f32  * 'f33 ) row ) face;
         u:( ('u11  * 'u12  * 'u13 ) row *
             ('u21  * 'u22  * 'u23 ) row *
             ('u31  * 'u32  * 'u33 ) row ) face;
         b:( ('b11  * 'b12  * 'b13 ) row *
             ('b21  * 'b22  * 'b23 ) row *
             ('b31  * 'b32  * 'b33 ) row ) face;
         d:( ('d11  * 'd12  * 'd13 ) row *
             ('d21  * 'd22  * 'd23 ) row *
             ('d31  * 'd32  * 'd33 ) row ) face;
         l:( ('l11  * 'l12  * 'l13 ) row *
             ('l21  * 'l22  * 'l23 ) row *
             ('l31  * 'l32  * 'l33 ) row ) face;
         r: 'r face;
        > cube,

        <f:( ('d31  * 'f12  * 'f13 ) row *
             ('d21  * 'f22  * 'f23 ) row *
             ('d11  * 'f32  * 'f33 ) row ) face;
         u:( ('f31  * 'u12  * 'u13 ) row *
             ('f21  * 'u22  * 'u23 ) row *
             ('f11  * 'u32  * 'u33 ) row ) face;
         b:( ('u11  * 'b12  * 'b13 ) row *
             ('u21  * 'b22  * 'b23 ) row *
             ('u31  * 'b32  * 'b33 ) row ) face;
         d:( ('b11  * 'd12  * 'd13 ) row *
             ('b21  * 'd22  * 'd23 ) row *
             ('b31  * 'd32  * 'd33 ) row ) face;
         l:( ('l13  * 'l23  * 'l33 ) row *
             ('l12  * 'l22  * 'l32 ) row *
             ('l11  * 'l21  * 'l31 ) row ) face;
         r: 'r face;
        > cube
      ) t


end open Action

type 'a t =
  | [] : init cube t
  | (::): ('a,'b) Action.t  * 'a t -> 'b t

let x: init cube t = [F;F;F;F]
let y: init cube t = [U;U;U;U]
let z: init cube t = [D;D;D;D]
let w: init cube t = [L;L;L;L]

let m = [D]

type truth = T

let bridge: 'a t -> 'a t -> truth = fun _ _ -> T

module type t = sig type t end

let lift (type t) (x:t): (module t with type t = t ) =
  (module struct type nonrec t = t end)


module Point = (val lift [F;U;U;D;L;L;L])

type (_,_) eq = Refl:('a,'a) eq

let dimaeter_is_greater_than_two: type a. a -> (Point.t,a) eq -> truth =
  fun x eq -> match eq, x with
  | Refl, _::_::_::_::_::_::_::_ -> T
  | _ -> .

type alt = private A1_or_2

type _1 = private A1
type _2 = private A2
type _3 = private A3
type _4 = private A4
type _5 = private A5
type _6 = private A6
type _7 = private A7
type _8 = private A8
type _9 = private A9
type _10 = private A10
type _11 = private A11
type _12 = private A12



type stop = private Stop
type z = private Z
type 'a s = private S

module S8 = struct

  type (_, _,_) n=
    | N1: ( _1, 'a, 'a *  _ *  _ *  _ *  _ *  _ *  _ *  _ ) n
    | N2: ( _2, 'a,  _ * 'a *  _ *  _ *  _ *  _ *  _ *  _ ) n
    | N3: ( _3, 'a,  _ *  _ * 'a *  _ *  _ *  _ *  _ *  _ ) n
    | N4: ( _4, 'a,  _ *  _ *  _ * 'a *  _ *  _ *  _ *  _ ) n
    | N5: ( _5, 'a,  _ *  _ *  _ *  _ * 'a *  _ *  _ *  _ ) n
    | N6: ( _6, 'a,  _ *  _ *  _ *  _ *  _ * 'a *  _ *  _ ) n
    | N7: ( _7, 'a,  _ *  _ *  _ *  _ *  _ *  _ * 'a *  _ ) n
    | N8: ( _9, 'a,  _ *  _ *  _ *  _ *  _ *  _ *  _ * 'a ) n

  type (_,_,_) p =
    | []: (stop, z , _ ) p
    | (::): ('nat, 'counter, 'rev) n * ('dir,'counter,'rev) p ->
      ('nat * 'dir, 'counter s, 'rev) p

    let z = [N1;N2;N3;N4;N5;N6;N7;N8]
    let p = [N2;N4;N6;N5;N7;N8]
end
