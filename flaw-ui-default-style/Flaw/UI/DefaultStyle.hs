{-|
Module: Flaw.UI.DefaultStyle
Description: Just some single UI style (fonts, metrics and colors).
License: MIT
-}

module Flaw.UI.DefaultStyle
	( defaultStyleMetrics
	, initDefaultStyleDrawer
	) where

import Control.Concurrent.STM
import Data.Char

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.FreeType
import Flaw.Graphics.Font.Harfbuzz
import Flaw.Graphics.Font.Render
import Flaw.Graphics.Font.Util
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.UI.DefaultStyle.Data
import Flaw.UI.Drawer
import Flaw.UI.Metrics

defaultStyleMetrics :: Metrics
defaultStyleMetrics = Metrics
	{ metricsGap = 1
	, metricsBigGap = 3
	, metricsFrameClient = Vec4 5 25 5 5
	, metricsFrameTopBorder = 5
	, metricsMainWidth = 250
	, metricsButtonSize = Vec2 80 24
	, metricsEditBoxHeight = 24
	, metricsLabelSize = Vec2 100 20
	, metricsTitleHeight = 25
	, metricsSliderPieceWidth = 10
	, metricsSliderHeight = 20
	, metricsPileBoxGripWidth = 6
	, metricsListBoxColumnHeaderHeight = 20
	, metricsListBoxItemHeight = 20
	, metricsScrollBarWidth = 16
	}

initDefaultStyleDrawer :: Device d => d -> IO (Drawer d, IO ())
initDefaultStyleDrawer device = withSpecialBook $ \bk -> do

	-- create glyph renderer
	glyphRenderer <- book bk $ initGlyphRenderer device GlyphSubpixelModeHorizontalRGB

	-- create free type library
	freeTypeLibrary <- book bk initFreeType

	-- embedded font
	fontData <- loadFontData
	let loadFont size xscale yscale = do
		font <- book bk $ loadFreeTypeFont freeTypeLibrary size fontData
		fontShaper <- book bk $ createHarfbuzzShaper font
		renderableFontCache <- book bk $ createRenderableFontCache device $ \glyphsIndices -> do
			glyphs@Glyphs
				{ glyphsTextureInfo = TextureInfo
					{ textureHeight = height
					}
				} <- makeScaledGlyphs (createFreeTypeGlyphs font) xscale yscale GlyphUnionConfig
				{ glyphUnionConfigWidth = 2048
				, glyphUnionConfigBorderX = 2 + xscale
				, glyphUnionConfigBorderY = 2 + yscale
				, glyphUnionConfigHeightIsPowerOfTwo = True
				} glyphsIndices
			return $ if height <= 2048 then Just glyphs else Nothing
		return DrawerFont
			{ drawerFontRenderableFontCache = renderableFontCache
			, drawerFontShaper = SomeFontShaper fontShaper
			}

	-- label font
	labelFont <- loadFont 12 2 1
	-- title font
	titleFont <- loadFont 16 2 1

	-- create canvas
	canvas <- book bk $ initCanvas device

	-- color in SRGB space
	let color q = case q of
		[r1, r2, g1, g2, b1, b2, a1, a2] -> Vec4 (corr $ z r1 r2) (corr $ z g1 g2) (corr $ z b1 b2) (z a1 a2)
		_ -> error "wrong color format"
		where
			z c1 c2 = fromIntegral (h c1 * 16 + h c2) / 255
			h c
				| isDigit c = ord c - ord '0'
				| c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
				| otherwise = error "wrong hex symbol"
			corr c = exp $ log c * 2.2

	-- styles
	let styles = DrawerStyles
		{ drawerMetrics = defaultStyleMetrics
		, drawerLabelFont = labelFont
		, drawerEditFont = labelFont
		, drawerTitleFont = titleFont
		, drawerFlatStyleVariant = StyleVariant
			{ styleVariantNormalStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "30304cff"
				, styleBorderColor = color "000020ff"
				}
			, styleVariantMousedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			, styleVariantPressedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			, styleVariantSelectedFocusedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			, styleVariantSelectedUnfocusedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			}
		, drawerLoweredStyleVariant = StyleVariant
			{ styleVariantNormalStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "12122bff"
				, styleBorderColor = color "ffffff80"
				}
			, styleVariantMousedStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "12122bff"
				, styleBorderColor = color "ffa500ff"
				}
			, styleVariantPressedStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "12122bff"
				, styleBorderColor = color "ffa500ff"
				}
			, styleVariantSelectedFocusedStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "808080ff"
				, styleBorderColor = color "ffffffff"
				}
			, styleVariantSelectedUnfocusedStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "404040ff"
				, styleBorderColor = color "ffffff00"
				}
			}
		, drawerRaisedStyleVariant = StyleVariant
			{ styleVariantNormalStyle = Style
				{ styleTextColor = color "ffffffff"
				, styleFillColor = color "515182ff"
				, styleBorderColor = color "ffffff80"
				}
			, styleVariantMousedStyle = Style
				{ styleTextColor = color "ffa500ff"
				, styleFillColor = color "515182ff"
				, styleBorderColor = color "ffa500ff"
				}
			, styleVariantPressedStyle = Style
				{ styleTextColor = color "000000ff"
				, styleFillColor = color "ffa500ff"
				, styleBorderColor = color "ffffffff"
				}
			, styleVariantSelectedFocusedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			, styleVariantSelectedUnfocusedStyle = Style
				{ styleTextColor = color "112233ff"
				, styleFillColor = color "112233ff"
				, styleBorderColor = color "112233ff"
				}
			}
		, drawerFrameOuterNormalStyle = Style
			{ styleTextColor = color "000020ff"
			, styleFillColor = color "c0c0ffff"
			, styleBorderColor = color "000020ff"
			}
		, drawerFrameOuterFocusedStyle = Style
			{ styleTextColor = color "000020ff"
			, styleFillColor = color "e7e7ffff"
			, styleBorderColor = color "000020ff"
			}
		, drawerFrameInnerStyle = Style
			{ styleTextColor = color "ffffffff"
			, styleFillColor = color "30304cff"
			, styleBorderColor = color "000020ff"
			}
		}

	-- create drawer finally
	atomically $ initDrawer canvas glyphRenderer styles
