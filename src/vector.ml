


let eps = sqrt epsilon_float

module type VECTOR =
  sig
    type vector

    val vzero : vector
    val plus : vector -> vector -> vector
    val minus : vector -> vector -> vector
    val mult : float -> vector -> vector
    val dot : vector -> vector -> float
    val cross : vector -> vector -> vector
    val len : vector -> float
    val normalize : vector -> vector
    val angle : vector -> vector -> float
    val max : vector -> vector -> vector
  end


module Vector : VECTOR with type vector = float * float * float =
  struct
    
    type vector = float * float * float

    let vzero = (0.,0.,0.)
    let plus (x1,y1,z1) (x2,y2,z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
    let minus (x1,y1,z1) (x2,y2,z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
    let mult a (x,y,z) = (a *. x, a *. y, a*.z)
    let dot (x1,y1,z1) (x2,y2,z2) = x1*.x2 +. y1*.y2 +. z1*.z2
    let cross (x1,y1,z1) (x2,y2,z2) = (y1 *. z2 -. z1 *. y2, z1 *. x2 -. x1 *. z2, x1 *. y2 -. y1 *. x2)
    let len v = sqrt (dot v v)
    let normalize (x, y, z) = let l = len (x,y,z) in (x/.l, y/.l, z/.l)
    let angle v w = acos ( dot v w /. (len v *. len w) )
    let max (x1, y1, z1) (x2, y2, z2) = ( (if x1 < x2 then x2 else x1),
                                          (if y1 < y2 then y2 else y1),
                                          (if z1 < z2 then z2 else z1))                  
  end