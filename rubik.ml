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


end open Action

type 'a t =
  | [] : init cube t
  | (::): ('a,'b) Action.t  * 'a t -> 'b t

let x: init cube t = [F;F;F;F]
let y: init cube t = [U;U;U;U]
