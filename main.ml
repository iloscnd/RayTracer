
open Vector
open Camera
open Item



(*Ponizszy kod parsuje argumenty uruchomienia programu i wywoluje funkcje Camera.getImage generujaca obraz*)


let check_size x y =
    try
        (int_of_string x, int_of_string y)
    with
    | _ -> print_string ("Values for size have to be integer.\n"); exit(1)

let check_float x arg = 
    try
        float_of_string x
    with
    | _ -> print_string ("Values for " ^ arg ^ "have to be floats.\n");exit(1)


let print_help program_name = 
    print_string(
    "RayTracer\nUSE\n" ^
    program_name ^ 
    "\n\t --help\n" ^
    "\t\t Show summary of options\n" ^
    "\t --no-display\n" ^
    "\t\t Genereted image will not be printed on screen\n" ^
    "\t --size width hight\n" ^
    "\t\t Changes image size to width x hight\n" ^
    "\t --lens x y z\n" ^
    "\t\t Specify position of lens\n" ^
    "\t --items path\n" ^
    "\t\t Specify path to json file with description of the scene\n" ^
    "\t --lights path\n" ^
    "\t\t Specify path to json file with description of the lights\n" ^
    "\t --scale val\n" ^
    "\t\t Specify step size of pixel\n" ^
    "\t --gamma val\n" ^
    "\t\t Specify value of gamma in gamma correction\n" ^
    "\t --out-file path\n" ^
    "\t\t Saves file in path\n"
    )



let _ = if not !Sys.interactive 
then ( 
    let argv = Array.to_list Sys.argv
    and items = ref Item.world
    and lights = ref Item.lights
    and imgX = ref 1000
    and imgY = ref 1000
    and scale = ref 0.002
    and lens = ref (1.,1.,1.)
    and gamma = ref 1.5
    and (outFile: string option ref) = ref None
    and display = ref true
    in let rec argsParser = function
    | ("--help"::_) -> print_help (List.hd argv); exit(0)
    | ("--no-display"::tl) ->   display := false;
                                argsParser tl
    | ("--size"::x::y::tl) ->   let x,y = check_size x y in
                                imgX := x; 
                                imgY := y;
                                argsParser tl
    | ("--items"::path::tl) ->  items := read_items path;
                                argsParser tl
    | ("--lights"::path::tl) -> lights := read_lights path;
                                argsParser tl
    | ("--scale"::x::tl) ->     scale := check_float x "--scale";
                                argsParser tl
    | ("--gamma"::x::tl) ->     gamma := check_float x "--gamma";
                                argsParser tl
    | ("--lens"::x::y::z::tl)-> lens := (check_float x "--lens", check_float y "--lens", check_float z "--lens");
                                argsParser tl
    | ("--out-file"::path::tl)-> outFile := Some path;
                                argsParser tl
    | (x::[]) -> print_string ("Need value for " ^ x ^ " argument.\n"); exit(1);
    | ("--size"::_::[]) -> print_string ("Need second value for --size argument.\n"); exit(1);
    | ("--lens"::_::[])| ("--lens"::_::_::[]) -> print_string ("Need three values for --lens arugment\n"); exit(1);
    | [] -> (
            Graphics.open_graph "";
            Graphics.resize_window 1 1;
            let img =  (Camera.getImage (Camera.newCamera !lens !imgX !imgY !scale) !items !lights !gamma)
            in
            (match !outFile with
            | None -> ()
            | Some out -> Camera.saveImage img out );
            if !display then (
                Graphics.resize_window !imgX !imgY;
                Graphics.draw_image img 0 0; 
                ignore(Graphics.wait_next_event [Graphics.Key_pressed]));
            Graphics.close_graph ())
            

    | x::_-> print_string (
        "unrecognised option: " ^ x ^
        "\nUse " ^  (List.hd argv) ^  " --help to list options\n")

    in argsParser (List.tl argv)
        

)