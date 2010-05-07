(* Cairo_misc *)

open Cairo

let draw_path cr list =
	let rec draw_path = function
		| `MOVE_TO p -> Cairo.move_to_point cr p
		| `LINE_TO p -> Cairo.line_to_point cr p
		| `CLOSE -> Cairo.close_path cr
		| `CURVE_TO (p1, p2, p3) -> Cairo.curve_to_point cr p1 p2 p3
	in List.iter draw_path list

let measure_path cr list =
	Cairo.save cr;
		Cairo.new_path cr;
		draw_path cr list;
	let extents = Cairo.fill_extents cr in
		Cairo.new_path cr;
	Cairo.restore cr;
	extents
