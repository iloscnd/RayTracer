open Vector
       
type item =
  | Sphere of Vector.vector * float (* center, radius *)
  | Plane of Vector.vector * Vector.vector (* position, normal vector *)

type surface =
  | Opaque

type color = Vector.vector 

type light =
  | PointLight of Vector.vector * color (* position *)
  | SunLight of Vector.vector * color (* direction *)



(* ponizsze funkcje czytaja za pomoca biblioteki Yojson pliki JSON z opisami odpowiednio sceny i zrodel swiatla *)


let read_items itemsLocation =
    try
        let json = Yojson.Basic.from_file itemsLocation
        in 
        let rec read_file acc = function
        | [] -> acc
        | ("sphere", `List [`Float x; `Float y; `Float z; `Float r])::tl -> 
                                   read_file ((Sphere ((x,y,z), r),Opaque)::acc) tl

        | ("sphere", format)::_ -> print_string ("Bad formatting for sphere.\n"); exit(1);
      
        | ("plane", `List [`Float x1; `Float y1; `Float z1;
                           `Float x2; `Float y2; `Float z2])::tl -> 
                                   read_file ((Plane ((x1,y1,z1), Vector.normalize (x2,y2,z2)),Opaque)::acc) tl

        | ("plane", format)::_ -> print_string ("Bad formatting for plane.\n"); exit(1);
      
        | x::_ -> print_string("Bad JSON entry " ^ fst x ^ ".\n"); exit(1)

        in match json with
        | `Assoc xs -> read_file [] xs
        | _ -> print_string ("Bad JSON in file " ^ itemsLocation ^".\n"); exit(1)
    with
    | Sys_error err_msg -> print_string (err_msg ^ "\n"); exit(1)
    | Yojson.Json_error _ -> print_string (itemsLocation ^ " is not valid JSON file\n"); exit(1)
    | _ -> failwith "Unknown"

let read_lights lightsLocation =
    try
        let json = Yojson.Basic.from_file lightsLocation
        in 
        let rec read_file acc = function
        | [] -> acc
        | ("pointLight", `List [`Float x; `Float y; `Float z;
                           `Float r; `Float g; `Float b])::tl -> 
                                   read_file ((PointLight ((x,y,z), (r,g,b)))::acc) tl

        | ("pointLight", format)::_ -> print_string ("Bad formatting for point light source.\n"); exit(1);

        | ("sunLight", `List [`Float x; `Float y; `Float z;
                           `Float r; `Float g; `Float b])::tl -> 
                                 Printf.printf "%f %f %f\n" x y z ;
                                   read_file ((SunLight (Vector.normalize (x,y,z), (r,g,b)))::acc) tl

        | ("sunLight", format)::_ -> print_string ("Bad formatting for sun light source.\n"); exit(1);

      
        | x::_ -> print_string("Bad json entry " ^ fst x ^ ".\n"); exit(1)

        in match json with
        | `Assoc xs -> read_file [] xs
        | _ -> print_string ("JSON in " ^ lightsLocation ^" is not associative array.\n"); exit(1)
    with
    | Sys_error err_msg -> print_string (err_msg ^ "\n"); exit(1)
    | Yojson.Json_error _ -> print_string (lightsLocation ^ " is not valid JSON file\n"); exit(1);
    | _ -> failwith "Unknown"
    





                     
let world =
  [ Sphere ((1.,1.,4.), 2.),Opaque;
    Plane ((0.,0.,5.), Vector.normalize (0.,0.,1.)), Opaque
  ]
let lights =
  [ PointLight ((0.1,0.1,0.1), (1.,1.,1.));
    PointLight ((-1.,1.,1.), (1., 0., 0.));
    PointLight ((2., 1., 1.), (0., 1., 0.))
  ]
