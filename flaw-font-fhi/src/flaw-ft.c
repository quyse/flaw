#include <ft2build.h>
#include FT_FREETYPE_H

FT_GlyphSlot flaw_ft_get_glyph_slot(FT_Face face)
{
	return face->glyph;
}

FT_Long flaw_ft_get_num_glyphs(FT_Face face)
{
	return face->num_glyphs;
}

FT_Bitmap* flaw_ft_get_bitmap(FT_GlyphSlot slot)
{
	return &slot->bitmap;
}

FT_Int flaw_ft_get_bitmap_left(FT_GlyphSlot slot)
{
	return slot->bitmap_left;
}

FT_Int flaw_ft_get_bitmap_top(FT_GlyphSlot slot)
{
	return slot->bitmap_top;
}
