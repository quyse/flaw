{-|
Module: Flaw.Graphics.Script
Description: Standard font scripts.
License: MIT
-}

module Flaw.Graphics.Font.Script
	( fontScript
	, fontScriptUnknown
	, fontScriptArabic
	, fontScriptArmenian
	, fontScriptAvestan
	, fontScriptBalinese
	, fontScriptBamum
	, fontScriptBassaVah
	, fontScriptBatak
	, fontScriptBengali
	, fontScriptBopomofo
	, fontScriptBrahmi
	, fontScriptBraille
	, fontScriptBuginese
	, fontScriptBuhid
	, fontScriptCanadianSyllabics
	, fontScriptCarian
	, fontScriptCaucasianAlbanian
	, fontScriptChakma
	, fontScriptCham
	, fontScriptCherokee
	, fontScriptCoptic
	, fontScriptCuneiform
	, fontScriptCypriot
	, fontScriptCyrillic
	, fontScriptDeseret
	, fontScriptDevanagari
	, fontScriptDuployan
	, fontScriptEgyptianHieroglyphs
	, fontScriptElbasan
	, fontScriptEthiopic
	, fontScriptGeorgian
	, fontScriptGlagolitic
	, fontScriptGothic
	, fontScriptGrantha
	, fontScriptGreek
	, fontScriptGujarati
	, fontScriptGurmukhi
	, fontScriptHan
	, fontScriptHangul
	, fontScriptHanunoo
	, fontScriptHebrew
	, fontScriptHiragana
	, fontScriptImperialAramaic
	, fontScriptInscriptionalPahlavi
	, fontScriptInscriptionalParthian
	, fontScriptJavanese
	, fontScriptKaithi
	, fontScriptKannada
	, fontScriptKatakana
	, fontScriptKayahLi
	, fontScriptKharoshthi
	, fontScriptKhmer
	, fontScriptKhojki
	, fontScriptKhudawadi
	, fontScriptLao
	, fontScriptLatin
	, fontScriptLepcha
	, fontScriptLimbu
	, fontScriptLinearA
	, fontScriptLinearB
	, fontScriptLisu
	, fontScriptLycian
	, fontScriptLydian
	, fontScriptMahajani
	, fontScriptMalayalam
	, fontScriptMandaic
	, fontScriptManichaean
	, fontScriptMeeteiMayek
	, fontScriptMendeKikakui
	, fontScriptMeroiticCursive
	, fontScriptMeroiticHieroglyphs
	, fontScriptMiao
	, fontScriptModi
	, fontScriptMongolian
	, fontScriptMro
	, fontScriptMyanmar
	, fontScriptNabataean
	, fontScriptNewTaiLue
	, fontScriptNko
	, fontScriptOgham
	, fontScriptOlChiki
	, fontScriptOldItalic
	, fontScriptOldNorthArabian
	, fontScriptOldPermic
	, fontScriptOldPersian
	, fontScriptOldSouthArabian
	, fontScriptOldTurkic
	, fontScriptOriya
	, fontScriptOsmanya
	, fontScriptPahawhHmong
	, fontScriptPalmyrene
	, fontScriptPauCinHau
	, fontScriptPhagsPa
	, fontScriptPhoenician
	, fontScriptPsalterPahlavi
	, fontScriptRejang
	, fontScriptRunic
	, fontScriptSamaritan
	, fontScriptSaurashtra
	, fontScriptSharada
	, fontScriptShavian
	, fontScriptSiddham
	, fontScriptSinhala
	, fontScriptSoraSompeng
	, fontScriptSundanese
	, fontScriptSylotiNagri
	, fontScriptSyriac
	, fontScriptTagalog
	, fontScriptTagbanwa
	, fontScriptTaiLe
	, fontScriptTaiTham
	, fontScriptTaiViet
	, fontScriptTakri
	, fontScriptTamil
	, fontScriptTelugu
	, fontScriptThaana
	, fontScriptThai
	, fontScriptTibetan
	, fontScriptTifinagh
	, fontScriptTirhuta
	, fontScriptUgaritic
	, fontScriptVai
	, fontScriptWarangCiti
	, fontScriptYi
	) where

import Flaw.Graphics.Font

import Data.Bits
import Data.Char

-- | Encode font script.
fontScript :: String -> FontScript
fontScript [a, b, c, d] = FontScript $ (fromIntegral $ ord a `shiftL` 24) .|. (fromIntegral $ ord b `shiftL` 16) .|. (fromIntegral $ ord c `shiftL` 8) .|. (fromIntegral $ ord d)
fontScript _ = error "wrong font script code"

fontScriptArabic               :: FontScript
fontScriptArabic                = fontScript "Arab"
fontScriptArmenian             :: FontScript
fontScriptArmenian              = fontScript "Armn"
fontScriptAvestan              :: FontScript
fontScriptAvestan               = fontScript "Avst"
fontScriptBalinese             :: FontScript
fontScriptBalinese              = fontScript "Bali"
fontScriptBamum                :: FontScript
fontScriptBamum                 = fontScript "Bamu"
fontScriptBassaVah             :: FontScript
fontScriptBassaVah              = fontScript "Bass"
fontScriptBatak                :: FontScript
fontScriptBatak                 = fontScript "Batk"
fontScriptBengali              :: FontScript
fontScriptBengali               = fontScript "Beng"
fontScriptBopomofo             :: FontScript
fontScriptBopomofo              = fontScript "Bopo"
fontScriptBrahmi               :: FontScript
fontScriptBrahmi                = fontScript "Brah"
fontScriptBraille              :: FontScript
fontScriptBraille               = fontScript "Brai"
fontScriptBuginese             :: FontScript
fontScriptBuginese              = fontScript "Bugi"
fontScriptBuhid                :: FontScript
fontScriptBuhid                 = fontScript "Buhd"
fontScriptCanadianSyllabics    :: FontScript
fontScriptCanadianSyllabics     = fontScript "Cans"
fontScriptCarian               :: FontScript
fontScriptCarian                = fontScript "Cari"
fontScriptCaucasianAlbanian    :: FontScript
fontScriptCaucasianAlbanian     = fontScript "Aghb"
fontScriptChakma               :: FontScript
fontScriptChakma                = fontScript "Cakm"
fontScriptCham                 :: FontScript
fontScriptCham                  = fontScript "Cham"
fontScriptCherokee             :: FontScript
fontScriptCherokee              = fontScript "Cher"
fontScriptCoptic               :: FontScript
fontScriptCoptic                = fontScript "Copt"
fontScriptCuneiform            :: FontScript
fontScriptCuneiform             = fontScript "Xsux"
fontScriptCypriot              :: FontScript
fontScriptCypriot               = fontScript "Cprt"
fontScriptCyrillic             :: FontScript
fontScriptCyrillic              = fontScript "Cyrl"
fontScriptDeseret              :: FontScript
fontScriptDeseret               = fontScript "Dsrt"
fontScriptDevanagari           :: FontScript
fontScriptDevanagari            = fontScript "Deva"
fontScriptDuployan             :: FontScript
fontScriptDuployan              = fontScript "Dupl"
fontScriptEgyptianHieroglyphs  :: FontScript
fontScriptEgyptianHieroglyphs   = fontScript "Egyp"
fontScriptElbasan              :: FontScript
fontScriptElbasan               = fontScript "Elba"
fontScriptEthiopic             :: FontScript
fontScriptEthiopic              = fontScript "Ethi"
fontScriptGeorgian             :: FontScript
fontScriptGeorgian              = fontScript "Geor"
fontScriptGlagolitic           :: FontScript
fontScriptGlagolitic            = fontScript "Glag"
fontScriptGothic               :: FontScript
fontScriptGothic                = fontScript "Goth"
fontScriptGrantha              :: FontScript
fontScriptGrantha               = fontScript "Gran"
fontScriptGreek                :: FontScript
fontScriptGreek                 = fontScript "Grek"
fontScriptGujarati             :: FontScript
fontScriptGujarati              = fontScript "Gujr"
fontScriptGurmukhi             :: FontScript
fontScriptGurmukhi              = fontScript "Guru"
fontScriptHan                  :: FontScript
fontScriptHan                   = fontScript "Hani"
fontScriptHangul               :: FontScript
fontScriptHangul                = fontScript "Hang"
fontScriptHanunoo              :: FontScript
fontScriptHanunoo               = fontScript "Hano"
fontScriptHebrew               :: FontScript
fontScriptHebrew                = fontScript "Hebr"
fontScriptHiragana             :: FontScript
fontScriptHiragana              = fontScript "Hira"
fontScriptImperialAramaic      :: FontScript
fontScriptImperialAramaic       = fontScript "Armi"
fontScriptInscriptionalPahlavi :: FontScript
fontScriptInscriptionalPahlavi  = fontScript "Phli"
fontScriptInscriptionalParthian:: FontScript
fontScriptInscriptionalParthian = fontScript "Prti"
fontScriptJavanese             :: FontScript
fontScriptJavanese              = fontScript "Java"
fontScriptKaithi               :: FontScript
fontScriptKaithi                = fontScript "Kthi"
fontScriptKannada              :: FontScript
fontScriptKannada               = fontScript "Knda"
fontScriptKatakana             :: FontScript
fontScriptKatakana              = fontScript "Kana"
fontScriptKayahLi              :: FontScript
fontScriptKayahLi               = fontScript "Kali"
fontScriptKharoshthi           :: FontScript
fontScriptKharoshthi            = fontScript "Khar"
fontScriptKhmer                :: FontScript
fontScriptKhmer                 = fontScript "Khmr"
fontScriptKhojki               :: FontScript
fontScriptKhojki                = fontScript "Khoj"
fontScriptKhudawadi            :: FontScript
fontScriptKhudawadi             = fontScript "Sind"
fontScriptLao                  :: FontScript
fontScriptLao                   = fontScript "Laoo"
fontScriptLatin                :: FontScript
fontScriptLatin                 = fontScript "Latn"
fontScriptLepcha               :: FontScript
fontScriptLepcha                = fontScript "Lepc"
fontScriptLimbu                :: FontScript
fontScriptLimbu                 = fontScript "Limb"
fontScriptLinearA              :: FontScript
fontScriptLinearA               = fontScript "Lina"
fontScriptLinearB              :: FontScript
fontScriptLinearB               = fontScript "Linb"
fontScriptLisu                 :: FontScript
fontScriptLisu                  = fontScript "Lisu"
fontScriptLycian               :: FontScript
fontScriptLycian                = fontScript "Lyci"
fontScriptLydian               :: FontScript
fontScriptLydian                = fontScript "Lydi"
fontScriptMahajani             :: FontScript
fontScriptMahajani              = fontScript "Mahj"
fontScriptMalayalam            :: FontScript
fontScriptMalayalam             = fontScript "Mlym"
fontScriptMandaic              :: FontScript
fontScriptMandaic               = fontScript "Mand"
fontScriptManichaean           :: FontScript
fontScriptManichaean            = fontScript "Mani"
fontScriptMeeteiMayek          :: FontScript
fontScriptMeeteiMayek           = fontScript "Mtei"
fontScriptMendeKikakui         :: FontScript
fontScriptMendeKikakui          = fontScript "Mend"
fontScriptMeroiticCursive      :: FontScript
fontScriptMeroiticCursive       = fontScript "Merc"
fontScriptMeroiticHieroglyphs  :: FontScript
fontScriptMeroiticHieroglyphs   = fontScript "Mero"
fontScriptMiao                 :: FontScript
fontScriptMiao                  = fontScript "Plrd"
fontScriptModi                 :: FontScript
fontScriptModi                  = fontScript "Modi"
fontScriptMongolian            :: FontScript
fontScriptMongolian             = fontScript "Mong"
fontScriptMro                  :: FontScript
fontScriptMro                   = fontScript "Mroo"
fontScriptMyanmar              :: FontScript
fontScriptMyanmar               = fontScript "Mymr"
fontScriptNabataean            :: FontScript
fontScriptNabataean             = fontScript "Nbat"
fontScriptNewTaiLue            :: FontScript
fontScriptNewTaiLue             = fontScript "Talu"
fontScriptNko                  :: FontScript
fontScriptNko                   = fontScript "Nkoo"
fontScriptOgham                :: FontScript
fontScriptOgham                 = fontScript "Ogam"
fontScriptOlChiki              :: FontScript
fontScriptOlChiki               = fontScript "Olck"
fontScriptOldItalic            :: FontScript
fontScriptOldItalic             = fontScript "Ital"
fontScriptOldNorthArabian      :: FontScript
fontScriptOldNorthArabian       = fontScript "Narb"
fontScriptOldPermic            :: FontScript
fontScriptOldPermic             = fontScript "Perm"
fontScriptOldPersian           :: FontScript
fontScriptOldPersian            = fontScript "Xpeo"
fontScriptOldSouthArabian      :: FontScript
fontScriptOldSouthArabian       = fontScript "Sarb"
fontScriptOldTurkic            :: FontScript
fontScriptOldTurkic             = fontScript "Orkh"
fontScriptOriya                :: FontScript
fontScriptOriya                 = fontScript "Orya"
fontScriptOsmanya              :: FontScript
fontScriptOsmanya               = fontScript "Osma"
fontScriptPahawhHmong          :: FontScript
fontScriptPahawhHmong           = fontScript "Hmng"
fontScriptPalmyrene            :: FontScript
fontScriptPalmyrene             = fontScript "Palm"
fontScriptPauCinHau            :: FontScript
fontScriptPauCinHau             = fontScript "Pauc"
fontScriptPhagsPa              :: FontScript
fontScriptPhagsPa               = fontScript "Phag"
fontScriptPhoenician           :: FontScript
fontScriptPhoenician            = fontScript "Phnx"
fontScriptPsalterPahlavi       :: FontScript
fontScriptPsalterPahlavi        = fontScript "Phlp"
fontScriptRejang               :: FontScript
fontScriptRejang                = fontScript "Rjng"
fontScriptRunic                :: FontScript
fontScriptRunic                 = fontScript "Runr"
fontScriptSamaritan            :: FontScript
fontScriptSamaritan             = fontScript "Samr"
fontScriptSaurashtra           :: FontScript
fontScriptSaurashtra            = fontScript "Saur"
fontScriptSharada              :: FontScript
fontScriptSharada               = fontScript "Shrd"
fontScriptShavian              :: FontScript
fontScriptShavian               = fontScript "Shaw"
fontScriptSiddham              :: FontScript
fontScriptSiddham               = fontScript "Sidd"
fontScriptSinhala              :: FontScript
fontScriptSinhala               = fontScript "Sinh"
fontScriptSoraSompeng          :: FontScript
fontScriptSoraSompeng           = fontScript "Sora"
fontScriptSundanese            :: FontScript
fontScriptSundanese             = fontScript "Sund"
fontScriptSylotiNagri          :: FontScript
fontScriptSylotiNagri           = fontScript "Sylo"
fontScriptSyriac               :: FontScript
fontScriptSyriac                = fontScript "Syrc"
fontScriptTagalog              :: FontScript
fontScriptTagalog               = fontScript "Tglg"
fontScriptTagbanwa             :: FontScript
fontScriptTagbanwa              = fontScript "Tagb"
fontScriptTaiLe                :: FontScript
fontScriptTaiLe                 = fontScript "Tale"
fontScriptTaiTham              :: FontScript
fontScriptTaiTham               = fontScript "Lana"
fontScriptTaiViet              :: FontScript
fontScriptTaiViet               = fontScript "Tavt"
fontScriptTakri                :: FontScript
fontScriptTakri                 = fontScript "Takr"
fontScriptTamil                :: FontScript
fontScriptTamil                 = fontScript "Taml"
fontScriptTelugu               :: FontScript
fontScriptTelugu                = fontScript "Telu"
fontScriptThaana               :: FontScript
fontScriptThaana                = fontScript "Thaa"
fontScriptThai                 :: FontScript
fontScriptThai                  = fontScript "Thai"
fontScriptTibetan              :: FontScript
fontScriptTibetan               = fontScript "Tibt"
fontScriptTifinagh             :: FontScript
fontScriptTifinagh              = fontScript "Tfng"
fontScriptTirhuta              :: FontScript
fontScriptTirhuta               = fontScript "Tirh"
fontScriptUgaritic             :: FontScript
fontScriptUgaritic              = fontScript "Ugar"
fontScriptVai                  :: FontScript
fontScriptVai                   = fontScript "Vaii"
fontScriptWarangCiti           :: FontScript
fontScriptWarangCiti            = fontScript "Wara"
fontScriptYi                   :: FontScript
fontScriptYi                    = fontScript "Yiii"
