
open Cairo

let pi = atan 1. *. 4.

let quadratic_curve_to cr x1 y1 x2 y2 = 
    let p = Cairo.get_current_point cr in 
    Cairo.curve_to cr 
        (p.x +. 2. /. 3. *. (x1 -. p.x)) 
        (p.y +. 2. /. 3. *. (y1 -. p.y)) 
        (x2 +. 2. /. 3. *. (x1 -. x2)) 
        (y2 +. 2. /. 3. *. (y1 -. y2)) 
        x2 
        y2

let arc_segment cr xc yc th0 th1 rx ry rot = 
    let sin_th = sin (rot *. (pi /. 180.)) in 
    let cos_th = sin (rot *. (pi /. 180.)) in 
    let a00 =     cos_th *. rx in 
    let a01 = ~-. sin_th *. ry in 
    let a10 = sin_th *. rx in 
    let a11 = cos_th *. ry in 
    let th_half = 0.5 *. (th1 -. th0) in 
    let t = (8. /. 3.) *. sin (th_half *. 0.5) *. sin (th_half *. 0.5) /. sin (th_half) in 
    let x1 = xc +. cos th0 -. t *. sin th0 in 
    let y1 = yc +. sin th0 +. t *. cos th0 in 
    let x3 = xc +. cos th1 in 
    let y3 = yc +. sin th1 in 
    let x2 = x3 +. t *. sin th1 in 
    let y2 = y3 -. t *. cos th1 in 
    Cairo.curve_to cr 
        (a00 *. x1 +. a01 *. y1) 
        (a10 *. x1 +. a11 *. y1) 
        (a00 *. x2 +. a01 *. y2) 
        (a10 *. x2 +. a11 *. y2) 
        (a00 *. x3 +. a01 *. y3) 
        (a10 *. x3 +. a11 *. y3) 

let arc_to cr rx ry rot large sweep x y = 
    let rx = abs_float rx in 
    let ry = abs_float ry in 
    
    let p = Cairo.get_current_point cr in 
    
    let sin_th = sin (rot *. (pi /. 180.)) in 
    let cos_th = cos (rot *. (pi /. 180.)) in 
    
    let dx = (p.x -. x) /. 2. in 
    let dy = (p.y -. y) /. 2. in 
    let dx1 =     cos_th *. dx +. sin_th *. dy in 
    let dy1 = ~-. sin_th *. dx +. cos_th *. dy in 
    let pr1 = rx *. rx in 
    let pr2 = ry *. ry in 
    let px = dx1 *. dx1 in 
    let py = dy1 *. dy1 in 
    let check = px /. pr1 +. py /. pr2 in 
    let rx,ry = if check > 1. then 
        (rx *. sqrt check), (ry *. sqrt check)
		else
	rx,ry in
    
    let a00 = cos_th /. rx in 
        let a01 = sin_th /. rx in 
        let a10 = ~-. sin_th /. ry in 
        let a11 =     cos_th /. ry in 
        let x0 = a00 *. p.x +. a01 *. p.y in 
        let y0 = a10 *. p.x +. a11 *. p.y in 
        let x1 = a00 *. x +. a01 *. y in 
        let y1 = a10 *. x +. a11 *. y in 
        let d = (x1 -. x0) *. (x1 -. x0) +. (y1 -. y0) *. (y1 -. y0) in 
        let sfactor_sq = match 1. /. d -. 0.25 with 
            | n when n < 0. -> 0. 
            | n -> sqrt n 
        in 
        let sfactor = if sweep = large then 
                ~-. (sqrt sfactor_sq) 
            else 
                sqrt sfactor_sq 
        in 
        let xc = 0.5 *. (x0 +. x1) -. sfactor *. (y1 -. y0) in 
        let yc = 0.5 *. (y0 +. y1) +. sfactor *. (x1 -. x0) in 
        
        let th0 = atan2 (y0 -. yc) (x0 -. xc) in 
        let th1 = atan2 (y1 -. yc) (x1 -. xc) in 
        
        let th_arc = 
            let th_arc = th1 -. th0 in 
            match sweep with 
            | true  when th_arc < 0. -> th_arc +. 2. *. pi 
            | false when th_arc > 0. -> th_arc -. 2. *. pi 
            | _ -> th_arc 
        in 
        
        let n_segs = ceil (abs_float (th_arc /. (pi *. 0.5 +. 0.001))) in 
        
        for i = 0 to (int_of_float n_segs) - 1 do 
            arc_segment cr xc yc 
                (th0 +. (float i) *. th_arc /. n_segs) 
                (th0 +. (float i +. 1.) *. th_arc /. n_segs) 
                rx ry rot 
        done

(* SVG Path Commands *)
(*
  Uppercase = Absolute; Lowercase = Relative
  
  M/m: moveto: (x y)+
  Z/z: closepath: ()
  L/l: lineto: (x y)+
  H/h: horizontal lineto: x +
  V/v: vertical lineto: y +
  C/c: curveto: (x1 y1 x2 y2 x y)+
  S/s: smooth curveto: (x2 y2 x y)+
  Q/q: quadratic curveto: (x1 y1 x y)+
  T/t: smooth quadratic curveto: (x y)+
  A/a: elliptical arc: (rx ry rot large sweep x y)+
*)

let rec group2 = function a :: b :: tl -> (a,b) :: group2 tl | _ -> []
let rec group4 = function a :: b :: c :: d :: tl -> (a,b,c,d) :: group4 tl | _ -> []
let rec group6 = function a :: b :: c :: d :: e :: f :: tl -> (a,b,c,d,e,f) :: group6 tl | _ -> []
let rec group7 = function a :: b :: c :: d :: e :: f :: g :: tl -> (a,b,c,d,e,f,g) :: group7 tl | _ -> []

let rec post_process = function
| [] -> []
| (op, args) :: list -> (match op with
	| 'Z' | 'z' -> [`Close]
	| 'M' -> List.map (fun p -> `Move p) (group2 args)
	| 'L' -> List.map (fun p -> `Line p) (group2 args)
	| 'H' -> List.map (fun x -> `Hline x) args
	| 'V' -> List.map (fun y -> `Vline y) args
	| 'C' -> List.map (fun v -> `Curve v) (group6 args)
	| 'S' -> List.map (fun v -> `Scurve v) (group4 args)
	| 'Q' -> List.map (fun v -> `Quad v) (group4 args)
	| 'T' -> List.map (fun v -> `Squad v) (group2 args)
	| 'A' -> let args = group7 args in
		List.map (fun (a,b,c,d,e,f,g) -> `Arc (a,b,c,d=1.,e=1.,f,g)) args
	| 'm' -> List.map (fun p -> `Move_r p) (group2 args)
	| 'l' -> List.map (fun p -> `Line_r p) (group2 args)
	| 'h' -> List.map (fun x -> `Hline_r x) args
	| 'v' -> List.map (fun y -> `Vline_r y) args
	| 'c' -> List.map (fun v -> `Curve_r v) (group6 args)
	| 's' -> List.map (fun v -> `Scurve_r v) (group4 args)
	| 'q' -> List.map (fun v -> `Quad_r v) (group4 args)
	| 't' -> List.map (fun v -> `Squad_r v) (group2 args)
	| 'a' -> let args = group7 args in
		List.map (fun (a,b,c,d,e,f,g) -> `Arc_r (a,b,c,d=1.,e=1.,f,g)) args
	| _ -> assert false)
	:: post_process list

let to_float list =
	let to_float = function
	| `Close -> `Close
	| `Move (x,y) -> `Move (float x,float y)
	| `Line (x,y) -> `Line (float x,float y)
	| `Hline x -> `Hline (float x)
	| `Vline y -> `Vline (float y)
	| `Curve (a,b,c,d,e,f) -> `Curve (float a,float b,float c,float d,float e,float f)
	| `Scurve (a,b,c,d) -> `Scurve (float a,float b,float c,float d)
	| `Quad (a,b,c,d) -> `Quad (float a,float b,float c,float d)
	| `Squad (a,b) -> `Squad (float a,float b)
	| `Arc (a,b,c,d,e,f,g) -> `Arc (float a,float b,float c,d,e,float f,float g)
	| `Move_r (x,y) -> `Move_r (float x,float y)
	| `Line_r (x,y) -> `Line_r (float x,float y)
	| `Hline_r x -> `Hline_r (float x)
	| `Vline_r y -> `Vline_r (float y)
	| `Curve_r (a,b,c,d,e,f) -> `Curve_r (float a,float b,float c,float d,float e,float f)
	| `Scurve_r (a,b,c,d) -> `Scurve_r (float a,float b,float c,float d)
	| `Quad_r (a,b,c,d) -> `Quad_r (float a,float b,float c,float d)
	| `Squad_r (a,b) -> `Squad_r (float a,float b)
	| `Arc_r (a,b,c,d,e,f,g) -> `Arc_r (float a,float b,float c,d,e,float f,float g)
	in
	List.map to_float list


let parse_string string =
	List.flatten (post_process (Svg_path_parser.data Svg_path_lexer.data (Lexing.from_string string)))

let opt def = function
| None -> def
| Some x -> x

let reflect p = function
| None -> p
| Some p' -> { x = p.x +. (p.x -. p'.x); y = p.y +. (p.y -. p'.y) }

let svg_path cr data =
	(* data : command list *)
	let p = ref { x = 0.; y = 0.; } in
	let s = ref None in
	let t = ref None in
	let point x y = { x = x; y = y; } in
	let x' x = x +. !p.x in
	let y' y = y +. !p.y in
	
	let rec command = function
	| `Close -> Cairo.close_path cr
	| `Move (x,y) -> Cairo.move_to cr x y; p := point x y
	| `Line (x,y) -> Cairo.line_to cr x y; p := point x y
	| `Hline x -> Cairo.line_to cr x !p.y; p := { !p with x = x; }
	| `Vline y -> Cairo.line_to cr !p.x y; p := { !p with y = y; }
	| `Curve (x1,y1,x2,y2,x,y) -> Cairo.curve_to cr x1 y1 x2 y2 x y; p := point x y; s := Some (point x2 y2)
	| `Scurve (x2,y2,x,y) -> let xy = reflect !p !s in
		Cairo.curve_to cr xy.x xy.y x2 y2 x y;
		p := point x y;
		s := Some (point x2 y2)
	| `Quad (x1,y1,x,y) -> quadratic_curve_to cr x1 y1 x y; p := point x y; t := Some (point x1 y1)
	| `Squad (x,y) -> let xy = reflect !p !t in
		quadratic_curve_to cr xy.x xy.y x y;
		p := point x y
	| `Arc (rx,ry,rot,large,sweep,x,y) -> arc_to cr rx ry rot large sweep x y; p := point x y
	(* now the relative versions *)
	| `Move_r (x,y) -> command (`Move (x' x, y' y))
	| `Line_r (x,y) -> command (`Line (x' x, y' y))
	| `Hline_r x -> command (`Hline (x' x))
	| `Vline_r y -> command (`Vline (y' y))
	| `Curve_r (x1,y1,x2,y2,x,y) -> command (`Curve (x' x1, y' y1, x' x2, y' y2, x' x, y' y))
	| `Scurve_r (x2,y2,x,y) -> command (`Scurve (x' x2, y' y2, x' x, y' y))
	| `Quad_r (x1,y1,x,y) -> command (`Quad (x' x1, y' y1, x' x, y' y))
	| `Squad_r (x,y) -> command (`Squad (x' x, y' y))
	| `Arc_r (rx,ry,rot,large,sweep,x,y) -> command (`Arc (x' rx, y' ry, rot, large, sweep, x' x, y' y))
	in
	let command c = (match c with
		| `Curve _ | `Scurve _ | `Curve_r _ | `Scurve_r _ -> t := None
		| `Quad _ | `Squad _ | `Quad_r _ | `Squad_r _ -> s := None
		| _ -> s := None; t := None); 
		command c in
	Cairo.new_path cr;
	List.iter command data

let svg_path_from_string cr s = svg_path cr (parse_string s)

let copy_svg_path cr path =
	svg_path cr (parse_string path);
	List.rev (Cairo.fold_path cr (fun acc path -> path :: acc) [])
