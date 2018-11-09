open Vector
open Item

module type TRACER =
  sig
    type ray
    type intersection

    val newRay : Vector.vector -> Vector.vector -> ray
    val intersect : ray -> item -> intersection
    val trace : ray -> float -> (item*surface) list -> light list -> color
    val correction : color -> float -> int*int*int
  
  end


module Tracer : TRACER =
  struct
               
    type ray = Vector.vector * Vector.vector (*origin, direction*)
    type intersection = Vector.vector * float (* point, distance *)


    (*Przy sledzeniu promienia funkcja intersect zwraca typ intersection czyli pare zawierajaca informacje o odleglosci od poczatku
      promienia i punkcie trafienia. W przypadku gdy promien nie trafia w rozwazany obiekt zwracana jest para (vzero, inf)  

      Przy sledzeniu promienia poszukuje sie najblizszego trafienia a nastepnie z miejsca trafienia sprawdza, ktore swiatla sa widoczne oraz
      w jakiej odleglosci sie znajduja. Z tych informacji obliczany jest kolor. Poniewaz powierzchnia rozprasza swiatlo nie wywolywane jest rekursyjnie trace.
    *)


    let newRay from dir = (from, dir)
    let intersect (origin, dir) = function
      | Plane (p, n) -> if abs_float (Vector.dot dir n) < eps then (Vector.vzero, infinity)
                        else let d = (Vector.dot (Vector.minus p origin) n) /. (Vector.dot dir n) in
                             let int_point = Vector.plus (Vector.mult d dir) origin in
                             if d > 0. then (int_point, d)
                             else (Vector.vzero, infinity)
      | Sphere (c, r) -> let b = Vector.dot dir (Vector.minus origin c) in
                         let delta = b *. b -. (Vector.len (Vector.minus origin c)) ** 2. +. r *. r in
                         if delta < eps then (Vector.vzero, infinity) else
                           let d = if -1. *. b -. sqrt(delta) > eps then -1. *. b -. sqrt(delta)
                                   else -1. *. b +. sqrt(delta) in
                           if d > eps then (Vector.plus origin (Vector.mult d dir), d) else
                             (Vector.vzero, infinity)
                               


    let rec findLights dist pos items acc = function
      | [] -> acc
      | (PointLight (c, col))::tl -> if List.filter (fun (i,_) ->
                                             snd (intersect (pos, Vector.normalize (Vector.minus c pos)) i) < infinity) items = []
                                     then findLights dist pos items
                                             ((Vector.mult (1. /. (Vector.len (Vector.minus c pos) +. dist) ** 2.) col)::acc) tl
                                     else findLights dist pos items acc tl
      | (SunLight (dir, col))::tl -> if List.filter (fun (i,_) -> snd (intersect (pos, Vector.mult (-1.) dir) i) < infinity) items = []
      								 then findLights dist pos items (col::acc) tl 
                                     else findLights dist pos items acc tl
                           

    let rec trace rey dist items lights =
      let rec closest rey min surf = function
        | [] -> (min,surf)
        | (i, s)::tl -> let hit = intersect rey i in
                        if snd hit < snd min then closest rey hit s tl
                        else closest rey min surf tl
      in let ((hitPos, hitDist),surf) = closest rey (Vector.vzero, infinity) Opaque items in
         if hitDist = infinity then ( Vector.vzero )else
           let color = List.fold_left (Vector.max) Vector.vzero (findLights (dist +. hitDist) hitPos items [] lights) in
           match surf with
           | Opaque -> color




    (* Korekcja gamma i dodatkowo zmienia wartosci z float na int *)

    let correction (r,g,b) gamma =
      let gamma =  1. /. gamma in
      ((int_of_float (255. *. (r ** gamma) )),
       (int_of_float (255. *. (g ** gamma) )),
       (int_of_float (255. *. (b ** gamma) )))

	
end
    
	
