open Tracer
open Vector
open Item

module type CAMERA =
  sig

    type camera
    type ray
    val newCamera : Vector.vector -> int -> int -> float -> camera
    val getLens : camera -> Vector.vector
    val getScale : camera -> float
    val getRay : camera -> ray
    val getNextState : camera -> camera option
    val getSize : camera -> int*int

    val getImage : camera -> (item * surface) list -> light list -> float ->
    	Graphics.image
    val saveImage : Graphics.image -> string -> unit


  end
    

module Camera : CAMERA  with type ray=Tracer.ray=
  struct
    type camera = Vector.vector * int * int * float * int * int
    type ray = Tracer.ray

    (* 
    Typ kamery przedstawia jednoczesnie stan w iterowaniu po pikselach, tzn dwa ostatnie argumenty mowia ktory pixel jest aktualnie rozwazany 
    Dzieki temu getRay posiada tylko jeden argument - kamere, potrzebna jest jednak tez funkcja zwracajaca kolejny stan
    *)
    
    let newCamera lensPos xSize ySize scale =
      (lensPos, xSize, ySize, scale, xSize-1, 0)
                                                       
    let getLens (lens,_,_,_, _, _) = lens
                                 
    let getScale (_, _, _, scale, _, _) = scale
                         
    let getRay (lens, maxX, maxY, scale, x, y) =
      if maxX <= x || maxY <= y || x < 0 || y < 0 then invalid_arg (Printf.sprintf "x y: %d %d" x y) else
      let x = float_of_int x
      and y = float_of_int y
      in Tracer.newRay lens (Vector.normalize (Vector.minus lens (Vector.mult scale (x, y, 0.))))

    let getNextState (lens, maxX, maxY, scale, x, y) =
      let x, y = if x <= 0 then (maxX-1, y+1) else (x-1,y) in
      if y >= maxY
      then None
      else Some (lens, maxX, maxY, scale, x, y)

    let getSize (_,x,y,_,_,_) = (x,y)
                


    let getImageFromList ((_,maxX,maxY,_,_,_):camera) zs = 
    let arr = Array.make_matrix maxY maxX 0 in
    let rec fromArray x y = function
    | [] -> ()
    | (r,g,b)::zs ->  if x >= maxX then fromArray 0 (y+1) ((r,g,b)::zs)
    			else if y >= maxY then ()
    			else (arr.(y).(x) <- Graphics.rgb r g b; fromArray (x+1) y zs) 
    in Graphics.make_image (fromArray 0 0 zs; arr)


    let getImage camera items lights gamma =
	let rec iter acc camera = 
		match camera with
		| None -> acc
		| Some camera -> let color = Tracer.trace (getRay camera) 0. items lights in
		                 let (r,g,b) = Tracer.correction color gamma
		                 in iter ((r,g,b)::acc) (getNextState camera)
	in getImageFromList camera (List.rev_append (iter [] (Some camera)) [])	


	let saveImage img fileName = 
		let out = open_out fileName 
		and img = Graphics.dump_image img
		in
		Printf.fprintf out "P3\n %d %d\n 256\n" (Array.length img) (Array.length img.(0));
		for x = 0 to Array.length img - 1 do 
			for y = 0 to Array.length img.(x) -1 do 
				Printf.fprintf out "%d %d %d\n" (img.(x).(y)/256/256) (img.(x).(y)/256 mod 256) (img.(x).(y) mod 256)
			done
		done;
		close_out out

  end