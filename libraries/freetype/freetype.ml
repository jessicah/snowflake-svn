
(* a simplified interface to do basics of following C-code:

FT_GlyphSlot  slot = face->glyph;  /* a small shortcut */
  FT_UInt       glyph_index;
  int           pen_x, pen_y, n;


  ... initialize library ...
  ... create face object ...
  ... set character size ...

  pen_x = 300;
  pen_y = 200;

  for ( n = 0; n < num_chars; n++ )
  {
    /* load glyph image into the slot (erase previous one) */
    error = FT_Load_Char( face, text[n], FT_LOAD_RENDER );
    if ( error )
      continue;  /* ignore errors */

    /* now, draw to our target surface */
    my_draw_bitmap( &slot->bitmap,
                    pen_x + slot->bitmap_left,
                    pen_y - slot->bitmap_top );

    /* increment pen position */
    pen_x += slot->advance.x >> 6;
  }
*)

(* only values that we probably care about *)

open Bigarray

type face

(*type ft_face

type face = {
	face : ft_face;
	(*num_glyphs : int;*)
	family_name : string;
	style_name : string;
	(*slot : glyph_slot; (* the glyph slot *)*)
}*)

type glyph_slot = {
	advance_x : int;
	advance_y : int;
	bitmap : bitmap;
	bitmap_left : int;
	bitmap_top : int;
}

and bitmap = {
	rows : int;
	width : int;
	pitch : int;
	buffer : (int, int8_unsigned_elt, c_layout) Array2.t;
	num_grays : int;
	pixel_mode : pixel_mode;
}

and pixel_mode = PM_None | PM_Mono | PM_Gray | PM_Gray2 | PM_Gray4 | PM_LCD | PM_LCD_V
	(* likely only use Mono and Gray *)

module Internal = struct
	external init : unit -> unit = "ml_freetype_init"
	external uninit : unit -> unit = "ml_freetype_done"
	
	external new_face : string -> face * string * string = "ml_freetype_newface"
	
	(*external set_char_size : face -> int * int -> int * int -> unit
		= "ml_freetype_charsize"*)
	
	external set_pixel_size : face -> int -> unit
		= "ml_freetype_pixelsize"
	
	external load_char : face -> char -> glyph_slot
		= "ml_freetype_loadchar"

end

let inited = ref false

let init () =
	if not !inited then begin
		Internal.init ();
		inited := true
	end

let face buffer = Internal.new_face buffer

let draw_string plot position face string =
	init ();
	(*ExtString.String.fold_left begin fun pos char ->
			let slot = Internal.load_char face char in
			plot slot.bitmap
				(fst pos + slot.bitmap_left)
				(snd pos - slot.bitmap_top);
			(fst pos + slot.advance_x lsr 6, snd pos)
		end position string*)
	position
