
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <callback.h>
#include <fail.h>
#include <bigarray.h>

#include <stdio.h>

#include <ft2build.h>
#include FT_FREETYPE_H

FT_Library *library;

value ml_freetype_init()
{
	CAMLparam0();
	
	library = caml_stat_alloc(sizeof(FT_Library));
	
	if (FT_Init_FreeType(library)) {
		caml_failwith("FT_Init_FreeType");
	}
	
	CAMLreturn(Val_unit);
}
	
value ml_freetype_done()
{
	CAMLparam0();
	
	/*if (FT_Done_FreeType(library)) {
		caml_failwith("FT_Done_FreeType");
	}*/
	
	CAMLreturn(Val_unit);
}

/* here comes trixy bit! */
value ml_freetype_newface(value buffer)
{
	CAMLparam1(buffer);
	CAMLlocal1(res);
	
	FT_Face *face;
	
	int error;
	
	res = caml_alloc_tuple(3);
	
	if ( (face = caml_stat_alloc(sizeof(FT_Face))) == NULL) {
		caml_failwith("ml_freetype_newface: out of memory");
	}
	
	if (error = FT_New_Memory_Face(
				*(FT_Library*)library,
				String_val(buffer),
				caml_string_length(buffer),
				0, face))
	{
		dprintf("freetype error: 0x%02x\n", error);
		caml_failwith("FT_New_Memory_Face");
	}
	
	Store_field(res, 0, (value)face);
	Store_field(res, 1, caml_copy_string((*face)->family_name));
	Store_field(res, 2, caml_copy_string((*face)->style_name));
	
	CAMLreturn(res);
}

value ml_freetype_pixelsize(value face, value pixels)
{
	CAMLparam2(face, pixels);
	
	if (FT_Set_Pixel_Sizes( *(FT_Face *)face, Int_val(pixels), Int_val(pixels))) {
		caml_failwith("FT_Set_Pixel_Sizes");
	}
	
	CAMLreturn(Val_unit);
}

value ml_freetype_loadchar(value facev, value chr)
{
	CAMLparam2(facev,chr);
	CAMLlocal3(slot,bitmap,data);
	
	FT_Face face;
	
	slot = caml_alloc_tuple(5);
	bitmap = caml_alloc_tuple(6);
	
	face = *(FT_Face*) facev;
	
	dprintf("ml_freetype_loadchar: getting bitmap stuff\n");
	
	if (FT_Load_Char( face, Int_val(chr), FT_LOAD_RENDER)) {
		caml_failwith("FT_Load_Char");
	}
	
	dprintf("ml_freetype_loadchar: got; creating ocaml values\n");
	
	intnat dims[] = { face->glyph->bitmap.rows, abs(face->glyph->bitmap.pitch) };
	
	data = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 2,
			(void*)(face->glyph->bitmap.buffer), dims);	
	
	Store_field(bitmap, 0, Val_int(face->glyph->bitmap.rows));
	Store_field(bitmap, 1, Val_int(face->glyph->bitmap.width));
	Store_field(bitmap, 2, Val_int(face->glyph->bitmap.pitch));
	Store_field(bitmap, 3, data);
	Store_field(bitmap, 4, Val_int(face->glyph->bitmap.num_grays));
	Store_field(bitmap, 5, Val_int(face->glyph->bitmap.pixel_mode));
	
	Store_field(slot, 0, Val_int(face->glyph->advance.x));
	Store_field(slot, 1, Val_int(face->glyph->advance.y));
	Store_field(slot, 2, bitmap);
	Store_field(slot, 3, Val_int(face->glyph->bitmap_left));
	Store_field(slot, 4, Val_int(face->glyph->bitmap_top));
	
	dprintf("ml_freetype_loadchar: created ocaml values\n");
	CAMLreturn(slot);
}
