
module BDF = struct
	
	type bbox = {
		width : int;
		height : int;
		x_offset : int;
		y_offset : int;
	}
	
	type glyph = {
		dwidth : int * int;
		bbox : bbox;
		data : bool array array;
	}
	
	type weight = Normal | Bold
	
	type slant = Regular | Italic
	
	type font = {
		family : string;
		weight : weight;
		slant : slant;
		size : int;
		ascent : int;
		descent : int;
		glyphs : glyph option array;
		global_bbox : bbox;
	}
	
	open ExtList;;
open ExtString;;
(*
type bbox = {
	width: int;
	height: int;
	x_offset: int;
	y_offset: int;
};;

type glyph = {
	dwidth: int * int;
	bbox: bbox;
	data: bool array array;
};;

type weight = Normal | Bold;;

type slant = Regular | Italic;;

type font = {
	family: string;
	weight: weight;
	slant: slant;
	size: int;
	ascent: int;
	descent: int;
	glyphs: glyph option array;
	global_bbox: bbox;
};;*)

external id : 'a -> 'a = "%identity";;
let return a = a;;

let find_header headers string = List.find
		(fun str -> String.starts_with str string) headers;;

let load_glyph_data_row data w =
	let array = Array.create w false in
	let arr2 = Array.create (String.length data / 2) 0 in
	for i = 0 to String.length data / 2 - 1 do
		arr2.(i) <- Scanf.sscanf (String.sub data (i * 2) 2) "%x" id
	done;
	for i = 0 to w - 1 do
		array.(i) <- arr2.(i/8) land (1 lsl (7-(i mod 8))) <> 0;
	done;
	return array;;

let load_glyph_data data w h =
	let glyph = Array.create_matrix h w false in
	List.iteri (fun i s -> glyph.(i) <- load_glyph_data_row s w) data;
	return glyph;;

let load_glyph list =
	let header,data = List.takewhile ((<>) "BITMAP ") list, List.tl (List.dropwhile ((<>) "BITMAP ") list) in
	let find_header = find_header header in
	let dwidth = find_header "DWIDTH "
	and bbx    = find_header "BBX " in
	let bbox = Scanf.sscanf bbx "BBX %d %d %d %d%!" (fun w h x y -> { width = w; height = h; x_offset = x; y_offset = y; }) in {
		dwidth  = Scanf.sscanf dwidth "DWIDTH %d %d%!" (fun w h -> w,h);
		bbox    = bbox;
		data    = load_glyph_data data bbox.width bbox.height;
	};;

let load_glyphs list =
	let list = List.tl list in
	let glyphs = List.fold_right (fun string xl -> if string = "ENDCHAR" then []::xl else (string::(List.hd xl))::(List.tl xl)) list [[]] in
	let array = Array.init 65536 (fun _ -> None) in
	List.iter (fun glyph -> try Scanf.sscanf (List.hd (List.tl glyph)) "ENCODING %d" (fun i -> array.(i) <- Some (load_glyph glyph)) with _ -> ()) glyphs;
	return array;;

let load data =
	let header,data =
		let ix = ref None and i = ref 0 in
		while !ix = None do
			if String.starts_with data.(!i) "CHARS " then ix := Some !i else incr i
		done;
		Array.to_list (Array.sub data 0 (Option.get !ix)), Array.to_list (Array.sub data (Option.get !ix) (Array.length data - (Option.get !ix))) in
	let find_header = find_header header in
	let family  = find_header "FAMILY_NAME "
	and weight  = find_header "WEIGHT_NAME "
	and slant   = find_header "SLANT "
	and ascent  = find_header "FONT_ASCENT "
	and descent = find_header "FONT_DESCENT "
	and size    = find_header "SIZE "
	and bbx     = find_header "FONTBOUNDINGBOX "
	and glyphs  = load_glyphs data in {
		family  = Scanf.sscanf family "FAMILY_NAME %S%!" id;
		weight  = Scanf.sscanf weight "WEIGHT_NAME %S%!" (function "Bold" -> Bold | _ -> Normal);
		slant   = Scanf.sscanf slant "SLANT %S%!" (function "I" -> Italic | _ -> Regular);
		size    = Scanf.sscanf size "SIZE %d %d %d%!" (fun pt _ _ -> pt);
		ascent  = Scanf.sscanf ascent "FONT_ASCENT %d%!" id;
		descent = Scanf.sscanf descent "FONT_DESCENT %d%!" id;
		global_bbox    = Scanf.sscanf bbx "FONTBOUNDINGBOX %d %d %d %d%!" (fun w h x y -> { width = w; height = h; x_offset = x; y_offset = y; });
		glyphs  = glyphs;
	};;
	
	(* some reason, the slant field is always Regular, so hack around to fix it *)
	let fonts =
		let f1 = Marshal.from_string FontData.courO14 0 in
		let f2 = Marshal.from_string FontData.courBO14 0 in
		ref [
			Marshal.from_string FontData.courR14 0;
			Marshal.from_string FontData.courB14 0;
			{ f1 with slant = Italic };
			{ f2 with slant = Italic };
			load (Marshal.from_string FontData.unifont 0);
		]
	
	let get family weight slant size =
		List.find begin fun font ->
				font.family = family && font.weight = weight && font.slant = slant && font.size = size
			end !fonts
end

open BDF
open Bigarray
open ExtString

(* plots a pixel at coords [x,y] with colour [r,g,b] *)
let plot fb x y (r,g,b) =
	if y >= 0 && y < Array2.dim1 fb && x >= 0 && x < Array2.dim2 fb then
		fb.{y,x} <- Int32.of_int ((r lsl 16) lor (g lsl 8) lor b)

let draw_uchar fb code font (px,py) fg bg =
	match font.glyphs.(code) with
	| None -> (px,py)
	| Some g ->
		for x = 0 to g.bbox.width - 1 do
			for y = 0 to g.bbox.height - 1 do
				if g.data.(y).(x) then
					plot fb
						(px + x + g.bbox.x_offset)
						(py - (g.bbox.height - y) - g.bbox.y_offset)
						fg
				else
					plot fb
						(px + x + g.bbox.x_offset)
						(py - (g.bbox.height - y) - g.bbox.y_offset)
						bg;
			done;
		done;
		fst g.dwidth, snd g.dwidth

(* draws the specified character from given font at [px,py] with [colour],
		and returns the position to use for drawing the next character *)
let draw_char fb ch font (px,py) colour =
	draw_uchar fb (int_of_char ch) font (px,py) colour (0,0,0)
	(*match font.glyphs.(int_of_char ch) with
	| None -> (px,py)
	| Some g ->
		for x = 0 to g.bbox.width - 1 do
			for y = 0 to g.bbox.height - 1 do
				if g.data.(y).(x) then
					plot fb
						(px + x + g.bbox.x_offset)
						(py - (g.bbox.height - y) - g.bbox.y_offset)
						colour;
			done;
		done;
		px + fst g.dwidth, py + snd g.dwidth*)

let draw_text fb text font origin colour =
	String.fold_left begin fun point ch ->
			draw_char fb ch font point colour 
		end origin text

let measure_text text font =
	String.fold_left begin fun width ch ->
			match font.glyphs.(int_of_char ch) with
			| None -> width
			| Some g -> width + fst g.dwidth
		end 0 text, font.ascent + font.descent
