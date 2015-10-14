using System;
using static SwinGameSDK.SwinGame;

namespace SwinGameSDK
{
    public class Color
    {
        private int _rgba;

        public Color(int rgba)
        {
            _rgba = rgba;
        }

        public byte R
        {
            get
            {
                return SwinGame.RedOf(this);
            }
        }

        public byte G
        {
            get
            {
                return SwinGame.GreenOf(this);
            }
        }

        public byte B
        {
            get
            {
                return SwinGame.BlueOf(this);
            }
        }

        public byte A
        {
            get
            {
                return SwinGame.TransparencyOf(this);
            }
        }

        public static Color FromArgb(int rgba)
        {
            return new Color(rgba);
        }

        public int ToArgb()
        {
            return _rgba;
        }

        public static bool operator == (Color c1, Color c2)
        {
          return  ((object)c1 == (object)c2) || ((((object)c1) != null) && (((object)c2) != null) && (c1._rgba == c2._rgba));
        }

        public static bool operator != (Color c1, Color c2)
        {
          return ! (c1 == c2);
        }

        public override bool Equals(object obj)
        {
          Color item = obj as Color;

          if (item == null)
          {
              return false;
          }

          return this._rgba == item._rgba;
        }

        public override int GetHashCode()
        {
            return this._rgba;
        }

        private static Color _Grey = null;
        public static Color Grey
        {
          get { if(_Grey == null) _Grey = RGBAColor(128, 128, 128, 255); return _Grey; }
        }

        private static Color _LightGrey = null;
        public static Color LightGrey
        {
          get { if(_LightGrey == null) _LightGrey = RGBAColor(200, 200, 200, 255); return _LightGrey; }
        }

        private static Color _Transparent = null;
        public static Color Transparent
        {
          get { if(_Transparent == null) _Transparent = RGBAColor(255, 255, 255, 0); return _Transparent; }
        }

        private static Color _AliceBlue = null;
        public static Color AliceBlue
        {
          get { if(_AliceBlue == null) _AliceBlue = RGBAColor(240, 248, 255, 255); return _AliceBlue; }
        }

        private static Color _AntiqueWhite = null;
        public static Color AntiqueWhite
        {
          get { if(_AntiqueWhite == null) _AntiqueWhite = RGBAColor(250, 235, 215, 255); return _AntiqueWhite; }
        }

        private static Color _Aqua = null;
        public static Color Aqua
        {
          get { if(_Aqua == null) _Aqua = RGBAColor(0, 255, 255, 255); return _Aqua; }
        }

        private static Color _Aquamarine = null;
        public static Color Aquamarine
        {
          get { if(_Aquamarine == null) _Aquamarine = RGBAColor(127, 255, 212, 255); return _Aquamarine; }
        }

        private static Color _Azure = null;
        public static Color Azure
        {
          get { if(_Azure == null) _Azure = RGBAColor(240, 255, 255, 255); return _Azure; }
        }

        private static Color _Beige = null;
        public static Color Beige
        {
          get { if(_Beige == null) _Beige = RGBAColor(245, 245, 220, 255); return _Beige; }
        }

        private static Color _Bisque = null;
        public static Color Bisque
        {
          get { if(_Bisque == null) _Bisque = RGBAColor(255, 228, 196, 255); return _Bisque; }
        }

        private static Color _Black = null;
        public static Color Black
        {
          get { if(_Black == null) _Black = RGBAColor(0, 0, 0, 255); return _Black; }
        }

        private static Color _BlanchedAlmond = null;
        public static Color BlanchedAlmond
        {
          get { if(_BlanchedAlmond == null) _BlanchedAlmond = RGBAColor(255, 235, 205, 255); return _BlanchedAlmond; }
        }

        private static Color _Blue = null;
        public static Color Blue
        {
          get { if(_Blue == null) _Blue = RGBAColor(0, 0, 255, 255); return _Blue; }
        }

        private static Color _BlueViolet = null;
        public static Color BlueViolet
        {
          get { if(_BlueViolet == null) _BlueViolet = RGBAColor(138, 43, 226, 255); return _BlueViolet; }
        }

        private static Color _Brown = null;
        public static Color Brown
        {
          get { if(_Brown == null) _Brown = RGBAColor(165, 42, 42, 255); return _Brown; }
        }

        private static Color _BurlyWood = null;
        public static Color BurlyWood
        {
          get { if(_BurlyWood == null) _BurlyWood = RGBAColor(222, 184, 135, 255); return _BurlyWood; }
        }

        private static Color _CadetBlue = null;
        public static Color CadetBlue
        {
          get { if(_CadetBlue == null) _CadetBlue = RGBAColor(95, 158, 160, 255); return _CadetBlue; }
        }

        private static Color _Chartreuse = null;
        public static Color Chartreuse
        {
          get { if(_Chartreuse == null) _Chartreuse = RGBAColor(127, 255, 0, 255); return _Chartreuse; }
        }

        private static Color _Chocolate = null;
        public static Color Chocolate
        {
          get { if(_Chocolate == null) _Chocolate = RGBAColor(210, 105, 30, 255); return _Chocolate; }
        }

        private static Color _Coral = null;
        public static Color Coral
        {
          get { if(_Coral == null) _Coral = RGBAColor(255, 127, 80, 255); return _Coral; }
        }

        private static Color _CornflowerBlue = null;
        public static Color CornflowerBlue
        {
          get { if(_CornflowerBlue == null) _CornflowerBlue = RGBAColor(100, 149, 237, 255); return _CornflowerBlue; }
        }

        private static Color _Cornsilk = null;
        public static Color Cornsilk
        {
          get { if(_Cornsilk == null) _Cornsilk = RGBAColor(255, 248, 220, 255); return _Cornsilk; }
        }

        private static Color _Crimson = null;
        public static Color Crimson
        {
          get { if(_Crimson == null) _Crimson = RGBAColor(220, 20, 60, 255); return _Crimson; }
        }

        private static Color _Cyan = null;
        public static Color Cyan
        {
          get { if(_Cyan == null) _Cyan = RGBAColor(0, 255, 255, 255); return _Cyan; }
        }

        private static Color _DarkBlue = null;
        public static Color DarkBlue
        {
          get { if(_DarkBlue == null) _DarkBlue = RGBAColor(0, 0, 139, 255); return _DarkBlue; }
        }

        private static Color _DarkCyan = null;
        public static Color DarkCyan
        {
          get { if(_DarkCyan == null) _DarkCyan = RGBAColor(0, 139, 139, 255); return _DarkCyan; }
        }

        private static Color _DarkGoldenrod = null;
        public static Color DarkGoldenrod
        {
          get { if(_DarkGoldenrod == null) _DarkGoldenrod = RGBAColor(184, 134, 11, 255); return _DarkGoldenrod; }
        }

        private static Color _DarkGray = null;
        public static Color DarkGray
        {
          get { if(_DarkGray == null) _DarkGray = RGBAColor(169, 169, 169, 255); return _DarkGray; }
        }

        private static Color _DarkGreen = null;
        public static Color DarkGreen
        {
          get { if(_DarkGreen == null) _DarkGreen = RGBAColor(0, 100, 0, 255); return _DarkGreen; }
        }

        private static Color _DarkKhaki = null;
        public static Color DarkKhaki
        {
          get { if(_DarkKhaki == null) _DarkKhaki = RGBAColor(189, 183, 107, 255); return _DarkKhaki; }
        }

        private static Color _DarkMagenta = null;
        public static Color DarkMagenta
        {
          get { if(_DarkMagenta == null) _DarkMagenta = RGBAColor(139, 0, 139, 255); return _DarkMagenta; }
        }

        private static Color _DarkOliveGreen = null;
        public static Color DarkOliveGreen
        {
          get { if(_DarkOliveGreen == null) _DarkOliveGreen = RGBAColor(85, 107, 47, 255); return _DarkOliveGreen; }
        }

        private static Color _DarkOrange = null;
        public static Color DarkOrange
        {
          get { if(_DarkOrange == null) _DarkOrange = RGBAColor(255, 140, 0, 255); return _DarkOrange; }
        }

        private static Color _DarkOrchid = null;
        public static Color DarkOrchid
        {
          get { if(_DarkOrchid == null) _DarkOrchid = RGBAColor(153, 50, 204, 255); return _DarkOrchid; }
        }

        private static Color _DarkRed = null;
        public static Color DarkRed
        {
          get { if(_DarkRed == null) _DarkRed = RGBAColor(139, 0, 0, 255); return _DarkRed; }
        }

        private static Color _DarkSalmon = null;
        public static Color DarkSalmon
        {
          get { if(_DarkSalmon == null) _DarkSalmon = RGBAColor(233, 150, 122, 255); return _DarkSalmon; }
        }

        private static Color _DarkSeaGreen = null;
        public static Color DarkSeaGreen
        {
          get { if(_DarkSeaGreen == null) _DarkSeaGreen = RGBAColor(143, 188, 139, 255); return _DarkSeaGreen; }
        }

        private static Color _DarkSlateBlue = null;
        public static Color DarkSlateBlue
        {
          get { if(_DarkSlateBlue == null) _DarkSlateBlue = RGBAColor(72, 61, 139, 255); return _DarkSlateBlue; }
        }

        private static Color _DarkSlateGray = null;
        public static Color DarkSlateGray
        {
          get { if(_DarkSlateGray == null) _DarkSlateGray = RGBAColor(47, 79, 79, 255); return _DarkSlateGray; }
        }

        private static Color _DarkTurquoise = null;
        public static Color DarkTurquoise
        {
          get { if(_DarkTurquoise == null) _DarkTurquoise = RGBAColor(0, 206, 209, 255); return _DarkTurquoise; }
        }

        private static Color _DarkViolet = null;
        public static Color DarkViolet
        {
          get { if(_DarkViolet == null) _DarkViolet = RGBAColor(148, 0, 211, 255); return _DarkViolet; }
        }

        private static Color _DeepPink = null;
        public static Color DeepPink
        {
          get { if(_DeepPink == null) _DeepPink = RGBAColor(255, 20, 147, 255); return _DeepPink; }
        }

        private static Color _DeepSkyBlue = null;
        public static Color DeepSkyBlue
        {
          get { if(_DeepSkyBlue == null) _DeepSkyBlue = RGBAColor(0, 191, 255, 255); return _DeepSkyBlue; }
        }

        private static Color _DimGray = null;
        public static Color DimGray
        {
          get { if(_DimGray == null) _DimGray = RGBAColor(105, 105, 105, 255); return _DimGray; }
        }

        private static Color _DodgerBlue = null;
        public static Color DodgerBlue
        {
          get { if(_DodgerBlue == null) _DodgerBlue = RGBAColor(30, 144, 255, 255); return _DodgerBlue; }
        }

        private static Color _Firebrick = null;
        public static Color Firebrick
        {
          get { if(_Firebrick == null) _Firebrick = RGBAColor(178, 34, 34, 255); return _Firebrick; }
        }

        private static Color _FloralWhite = null;
        public static Color FloralWhite
        {
          get { if(_FloralWhite == null) _FloralWhite = RGBAColor(255, 250, 240, 255); return _FloralWhite; }
        }

        private static Color _ForestGreen = null;
        public static Color ForestGreen
        {
          get { if(_ForestGreen == null) _ForestGreen = RGBAColor(34, 139, 34, 255); return _ForestGreen; }
        }

        private static Color _Fuchsia = null;
        public static Color Fuchsia
        {
          get { if(_Fuchsia == null) _Fuchsia = RGBAColor(255, 0, 255, 255); return _Fuchsia; }
        }

        private static Color _Gainsboro = null;
        public static Color Gainsboro
        {
          get { if(_Gainsboro == null) _Gainsboro = RGBAColor(220, 220, 220, 255); return _Gainsboro; }
        }

        private static Color _GhostWhite = null;
        public static Color GhostWhite
        {
          get { if(_GhostWhite == null) _GhostWhite = RGBAColor(248, 248, 255, 255); return _GhostWhite; }
        }

        private static Color _Gold = null;
        public static Color Gold
        {
          get { if(_Gold == null) _Gold = RGBAColor(255, 215, 0, 255); return _Gold; }
        }

        private static Color _Goldenrod = null;
        public static Color Goldenrod
        {
          get { if(_Goldenrod == null) _Goldenrod = RGBAColor(218, 165, 32, 255); return _Goldenrod; }
        }

        private static Color _Gray = null;
        public static Color Gray
        {
          get { if(_Gray == null) _Gray = RGBAColor(128, 128, 128, 255); return _Gray; }
        }

        private static Color _Green = null;
        public static Color Green
        {
          get { if(_Green == null) _Green = RGBAColor(0, 128, 0, 255); return _Green; }
        }

        private static Color _GreenYellow = null;
        public static Color GreenYellow
        {
          get { if(_GreenYellow == null) _GreenYellow = RGBAColor(173, 255, 47, 255); return _GreenYellow; }
        }

        private static Color _Honeydew = null;
        public static Color Honeydew
        {
          get { if(_Honeydew == null) _Honeydew = RGBAColor(240, 255, 240, 255); return _Honeydew; }
        }

        private static Color _HotPink = null;
        public static Color HotPink
        {
          get { if(_HotPink == null) _HotPink = RGBAColor(255, 105, 180, 255); return _HotPink; }
        }

        private static Color _IndianRed = null;
        public static Color IndianRed
        {
          get { if(_IndianRed == null) _IndianRed = RGBAColor(205, 92, 92, 255); return _IndianRed; }
        }

        private static Color _Indigo = null;
        public static Color Indigo
        {
          get { if(_Indigo == null) _Indigo = RGBAColor(75, 0, 130, 255); return _Indigo; }
        }

        private static Color _Ivory = null;
        public static Color Ivory
        {
          get { if(_Ivory == null) _Ivory = RGBAColor(255, 255, 240, 255); return _Ivory; }
        }

        private static Color _Khaki = null;
        public static Color Khaki
        {
          get { if(_Khaki == null) _Khaki = RGBAColor(240, 230, 140, 255); return _Khaki; }
        }

        private static Color _Lavender = null;
        public static Color Lavender
        {
          get { if(_Lavender == null) _Lavender = RGBAColor(230, 230, 250, 255); return _Lavender; }
        }

        private static Color _LavenderBlush = null;
        public static Color LavenderBlush
        {
          get { if(_LavenderBlush == null) _LavenderBlush = RGBAColor(255, 240, 245, 255); return _LavenderBlush; }
        }

        private static Color _LawnGreen = null;
        public static Color LawnGreen
        {
          get { if(_LawnGreen == null) _LawnGreen = RGBAColor(124, 252, 0, 255); return _LawnGreen; }
        }

        private static Color _LemonChiffon = null;
        public static Color LemonChiffon
        {
          get { if(_LemonChiffon == null) _LemonChiffon = RGBAColor(255, 250, 205, 255); return _LemonChiffon; }
        }

        private static Color _LightBlue = null;
        public static Color LightBlue
        {
          get { if(_LightBlue == null) _LightBlue = RGBAColor(173, 216, 230, 255); return _LightBlue; }
        }

        private static Color _LightCoral = null;
        public static Color LightCoral
        {
          get { if(_LightCoral == null) _LightCoral = RGBAColor(240, 128, 128, 255); return _LightCoral; }
        }

        private static Color _LightCyan = null;
        public static Color LightCyan
        {
          get { if(_LightCyan == null) _LightCyan = RGBAColor(224, 255, 255, 255); return _LightCyan; }
        }

        private static Color _LightGoldenrodYellow = null;
        public static Color LightGoldenrodYellow
        {
          get { if(_LightGoldenrodYellow == null) _LightGoldenrodYellow = RGBAColor(250, 250, 210, 255); return _LightGoldenrodYellow; }
        }

        private static Color _LightGreen = null;
        public static Color LightGreen
        {
          get { if(_LightGreen == null) _LightGreen = RGBAColor(144, 238, 144, 255); return _LightGreen; }
        }

        private static Color _LightGray = null;
        public static Color LightGray
        {
          get { if(_LightGray == null) _LightGray = RGBAColor(211, 211, 211, 255); return _LightGray; }
        }

        private static Color _LightPink = null;
        public static Color LightPink
        {
          get { if(_LightPink == null) _LightPink = RGBAColor(255, 182, 193, 255); return _LightPink; }
        }

        private static Color _LightSalmon = null;
        public static Color LightSalmon
        {
          get { if(_LightSalmon == null) _LightSalmon = RGBAColor(255, 160, 122, 255); return _LightSalmon; }
        }

        private static Color _LightSeaGreen = null;
        public static Color LightSeaGreen
        {
          get { if(_LightSeaGreen == null) _LightSeaGreen = RGBAColor(32, 178, 170, 255); return _LightSeaGreen; }
        }

        private static Color _LightSkyBlue = null;
        public static Color LightSkyBlue
        {
          get { if(_LightSkyBlue == null) _LightSkyBlue = RGBAColor(135, 206, 250, 255); return _LightSkyBlue; }
        }

        private static Color _LightSlateGray = null;
        public static Color LightSlateGray
        {
          get { if(_LightSlateGray == null) _LightSlateGray = RGBAColor(119, 136, 153, 255); return _LightSlateGray; }
        }

        private static Color _LightSteelBlue = null;
        public static Color LightSteelBlue
        {
          get { if(_LightSteelBlue == null) _LightSteelBlue = RGBAColor(176, 196, 222, 255); return _LightSteelBlue; }
        }

        private static Color _LightYellow = null;
        public static Color LightYellow
        {
          get { if(_LightYellow == null) _LightYellow = RGBAColor(255, 255, 224, 255); return _LightYellow; }
        }

        private static Color _Lime = null;
        public static Color Lime
        {
          get { if(_Lime == null) _Lime = RGBAColor(0, 255, 0, 255); return _Lime; }
        }

        private static Color _LimeGreen = null;
        public static Color LimeGreen
        {
          get { if(_LimeGreen == null) _LimeGreen = RGBAColor(50, 205, 50, 255); return _LimeGreen; }
        }

        private static Color _Linen = null;
        public static Color Linen
        {
          get { if(_Linen == null) _Linen = RGBAColor(250, 240, 230, 255); return _Linen; }
        }

        private static Color _Magenta = null;
        public static Color Magenta
        {
          get { if(_Magenta == null) _Magenta = RGBAColor(255, 0, 255, 255); return _Magenta; }
        }

        private static Color _Maroon = null;
        public static Color Maroon
        {
          get { if(_Maroon == null) _Maroon = RGBAColor(128, 0, 0, 255); return _Maroon; }
        }

        private static Color _MediumAquamarine = null;
        public static Color MediumAquamarine
        {
          get { if(_MediumAquamarine == null) _MediumAquamarine = RGBAColor(102, 205, 170, 255); return _MediumAquamarine; }
        }

        private static Color _MediumBlue = null;
        public static Color MediumBlue
        {
          get { if(_MediumBlue == null) _MediumBlue = RGBAColor(0, 0, 205, 255); return _MediumBlue; }
        }

        private static Color _MediumOrchid = null;
        public static Color MediumOrchid
        {
          get { if(_MediumOrchid == null) _MediumOrchid = RGBAColor(186, 85, 211, 255); return _MediumOrchid; }
        }

        private static Color _MediumPurple = null;
        public static Color MediumPurple
        {
          get { if(_MediumPurple == null) _MediumPurple = RGBAColor(147, 112, 219, 255); return _MediumPurple; }
        }

        private static Color _MediumSeaGreen = null;
        public static Color MediumSeaGreen
        {
          get { if(_MediumSeaGreen == null) _MediumSeaGreen = RGBAColor(60, 179, 113, 255); return _MediumSeaGreen; }
        }

        private static Color _MediumSlateBlue = null;
        public static Color MediumSlateBlue
        {
          get { if(_MediumSlateBlue == null) _MediumSlateBlue = RGBAColor(123, 104, 238, 255); return _MediumSlateBlue; }
        }

        private static Color _MediumSpringGreen = null;
        public static Color MediumSpringGreen
        {
          get { if(_MediumSpringGreen == null) _MediumSpringGreen = RGBAColor(0, 250, 154, 255); return _MediumSpringGreen; }
        }

        private static Color _MediumTurquoise = null;
        public static Color MediumTurquoise
        {
          get { if(_MediumTurquoise == null) _MediumTurquoise = RGBAColor(72, 209, 204, 255); return _MediumTurquoise; }
        }

        private static Color _MediumVioletRed = null;
        public static Color MediumVioletRed
        {
          get { if(_MediumVioletRed == null) _MediumVioletRed = RGBAColor(199, 21, 133, 255); return _MediumVioletRed; }
        }

        private static Color _MidnightBlue = null;
        public static Color MidnightBlue
        {
          get { if(_MidnightBlue == null) _MidnightBlue = RGBAColor(25, 25, 112, 255); return _MidnightBlue; }
        }

        private static Color _MintCream = null;
        public static Color MintCream
        {
          get { if(_MintCream == null) _MintCream = RGBAColor(245, 255, 250, 255); return _MintCream; }
        }

        private static Color _MistyRose = null;
        public static Color MistyRose
        {
          get { if(_MistyRose == null) _MistyRose = RGBAColor(255, 228, 225, 255); return _MistyRose; }
        }

        private static Color _Moccasin = null;
        public static Color Moccasin
        {
          get { if(_Moccasin == null) _Moccasin = RGBAColor(255, 228, 181, 255); return _Moccasin; }
        }

        private static Color _NavajoWhite = null;
        public static Color NavajoWhite
        {
          get { if(_NavajoWhite == null) _NavajoWhite = RGBAColor(255, 222, 173, 255); return _NavajoWhite; }
        }

        private static Color _Navy = null;
        public static Color Navy
        {
          get { if(_Navy == null) _Navy = RGBAColor(0, 0, 128, 255); return _Navy; }
        }

        private static Color _OldLace = null;
        public static Color OldLace
        {
          get { if(_OldLace == null) _OldLace = RGBAColor(253, 245, 230, 255); return _OldLace; }
        }

        private static Color _Olive = null;
        public static Color Olive
        {
          get { if(_Olive == null) _Olive = RGBAColor(128, 128, 0, 255); return _Olive; }
        }

        private static Color _OliveDrab = null;
        public static Color OliveDrab
        {
          get { if(_OliveDrab == null) _OliveDrab = RGBAColor(107, 142, 35, 255); return _OliveDrab; }
        }

        private static Color _Orange = null;
        public static Color Orange
        {
          get { if(_Orange == null) _Orange = RGBAColor(255, 165, 0, 255); return _Orange; }
        }

        private static Color _OrangeRed = null;
        public static Color OrangeRed
        {
          get { if(_OrangeRed == null) _OrangeRed = RGBAColor(255, 69, 0, 255); return _OrangeRed; }
        }

        private static Color _Orchid = null;
        public static Color Orchid
        {
          get { if(_Orchid == null) _Orchid = RGBAColor(218, 112, 214, 255); return _Orchid; }
        }

        private static Color _PaleGoldenrod = null;
        public static Color PaleGoldenrod
        {
          get { if(_PaleGoldenrod == null) _PaleGoldenrod = RGBAColor(238, 232, 170, 255); return _PaleGoldenrod; }
        }

        private static Color _PaleGreen = null;
        public static Color PaleGreen
        {
          get { if(_PaleGreen == null) _PaleGreen = RGBAColor(152, 251, 152, 255); return _PaleGreen; }
        }

        private static Color _PaleTurquoise = null;
        public static Color PaleTurquoise
        {
          get { if(_PaleTurquoise == null) _PaleTurquoise = RGBAColor(175, 238, 238, 255); return _PaleTurquoise; }
        }

        private static Color _PaleVioletRed = null;
        public static Color PaleVioletRed
        {
          get { if(_PaleVioletRed == null) _PaleVioletRed = RGBAColor(219, 112, 147, 255); return _PaleVioletRed; }
        }

        private static Color _PapayaWhip = null;
        public static Color PapayaWhip
        {
          get { if(_PapayaWhip == null) _PapayaWhip = RGBAColor(255, 239, 213, 255); return _PapayaWhip; }
        }

        private static Color _PeachPuff = null;
        public static Color PeachPuff
        {
          get { if(_PeachPuff == null) _PeachPuff = RGBAColor(255, 218, 185, 255); return _PeachPuff; }
        }

        private static Color _Peru = null;
        public static Color Peru
        {
          get { if(_Peru == null) _Peru = RGBAColor(205, 133, 63, 255); return _Peru; }
        }

        private static Color _Pink = null;
        public static Color Pink
        {
          get { if(_Pink == null) _Pink = RGBAColor(255, 192, 203, 255); return _Pink; }
        }

        private static Color _Plum = null;
        public static Color Plum
        {
          get { if(_Plum == null) _Plum = RGBAColor(221, 160, 221, 255); return _Plum; }
        }

        private static Color _PowderBlue = null;
        public static Color PowderBlue
        {
          get { if(_PowderBlue == null) _PowderBlue = RGBAColor(176, 224, 230, 255); return _PowderBlue; }
        }

        private static Color _Purple = null;
        public static Color Purple
        {
          get { if(_Purple == null) _Purple = RGBAColor(128, 0, 128, 255); return _Purple; }
        }

        private static Color _Red = null;
        public static Color Red
        {
          get { if(_Red == null) _Red = RGBAColor(255, 0, 0, 255); return _Red; }
        }

        private static Color _RosyBrown = null;
        public static Color RosyBrown
        {
          get { if(_RosyBrown == null) _RosyBrown = RGBAColor(188, 143, 143, 255); return _RosyBrown; }
        }

        private static Color _RoyalBlue = null;
        public static Color RoyalBlue
        {
          get { if(_RoyalBlue == null) _RoyalBlue = RGBAColor(65, 105, 225, 255); return _RoyalBlue; }
        }

        private static Color _SaddleBrown = null;
        public static Color SaddleBrown
        {
          get { if(_SaddleBrown == null) _SaddleBrown = RGBAColor(139, 69, 19, 255); return _SaddleBrown; }
        }

        private static Color _Salmon = null;
        public static Color Salmon
        {
          get { if(_Salmon == null) _Salmon = RGBAColor(250, 128, 114, 255); return _Salmon; }
        }

        private static Color _SandyBrown = null;
        public static Color SandyBrown
        {
          get { if(_SandyBrown == null) _SandyBrown = RGBAColor(244, 164, 96, 255); return _SandyBrown; }
        }

        private static Color _SeaGreen = null;
        public static Color SeaGreen
        {
          get { if(_SeaGreen == null) _SeaGreen = RGBAColor(46, 139, 87, 255); return _SeaGreen; }
        }

        private static Color _SeaShell = null;
        public static Color SeaShell
        {
          get { if(_SeaShell == null) _SeaShell = RGBAColor(255, 245, 238, 255); return _SeaShell; }
        }

        private static Color _Sienna = null;
        public static Color Sienna
        {
          get { if(_Sienna == null) _Sienna = RGBAColor(160, 82, 45, 255); return _Sienna; }
        }

        private static Color _Silver = null;
        public static Color Silver
        {
          get { if(_Silver == null) _Silver = RGBAColor(192, 192, 192, 255); return _Silver; }
        }

        private static Color _SkyBlue = null;
        public static Color SkyBlue
        {
          get { if(_SkyBlue == null) _SkyBlue = RGBAColor(135, 206, 235, 255); return _SkyBlue; }
        }

        private static Color _SlateBlue = null;
        public static Color SlateBlue
        {
          get { if(_SlateBlue == null) _SlateBlue = RGBAColor(106, 90, 205, 255); return _SlateBlue; }
        }

        private static Color _SlateGray = null;
        public static Color SlateGray
        {
          get { if(_SlateGray == null) _SlateGray = RGBAColor(112, 128, 144, 255); return _SlateGray; }
        }

        private static Color _Snow = null;
        public static Color Snow
        {
          get { if(_Snow == null) _Snow = RGBAColor(255, 250, 250, 255); return _Snow; }
        }

        private static Color _SpringGreen = null;
        public static Color SpringGreen
        {
          get { if(_SpringGreen == null) _SpringGreen = RGBAColor(0, 255, 127, 255); return _SpringGreen; }
        }

        private static Color _SteelBlue = null;
        public static Color SteelBlue
        {
          get { if(_SteelBlue == null) _SteelBlue = RGBAColor(70, 130, 180, 255); return _SteelBlue; }
        }

        private static Color _Tan = null;
        public static Color Tan
        {
          get { if(_Tan == null) _Tan = RGBAColor(210, 180, 140, 255); return _Tan; }
        }

        private static Color _Teal = null;
        public static Color Teal
        {
          get { if(_Teal == null) _Teal = RGBAColor(0, 128, 128, 255); return _Teal; }
        }

        private static Color _Thistle = null;
        public static Color Thistle
        {
          get { if(_Thistle == null) _Thistle = RGBAColor(216, 191, 216, 255); return _Thistle; }
        }

        private static Color _Tomato = null;
        public static Color Tomato
        {
          get { if(_Tomato == null) _Tomato = RGBAColor(255, 99, 71, 255); return _Tomato; }
        }

        private static Color _Turquoise = null;
        public static Color Turquoise
        {
          get { if(_Turquoise == null) _Turquoise = RGBAColor(64, 224, 208, 255); return _Turquoise; }
        }

        private static Color _Violet = null;
        public static Color Violet
        {
          get { if(_Violet == null) _Violet = RGBAColor(238, 130, 238, 255); return _Violet; }
        }

        private static Color _Wheat = null;
        public static Color Wheat
        {
          get { if(_Wheat == null) _Wheat = RGBAColor(245, 222, 179, 255); return _Wheat; }
        }

        private static Color _White = null;
        public static Color White
        {
          get { if(_White == null) _White = RGBAColor(255, 255, 255, 255); return _White; }
        }

        private static Color _WhiteSmoke = null;
        public static Color WhiteSmoke
        {
          get { if(_WhiteSmoke == null) _WhiteSmoke = RGBAColor(245, 245, 245, 255); return _WhiteSmoke; }
        }

        private static Color _Yellow = null;
        public static Color Yellow
        {
          get { if(_Yellow == null) _Yellow = RGBAColor(255, 255, 0, 255); return _Yellow; }
        }

        private static Color _YellowGreen = null;
        public static Color YellowGreen
        {
          get { if(_YellowGreen == null) _YellowGreen = RGBAColor(154, 205, 50, 255); return _YellowGreen; }
        }


    }
}