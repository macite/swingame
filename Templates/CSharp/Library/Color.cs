using System;
using SwinGameSDK.SwinGame;

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

        public static Color Grey
        {
          get { return RGBAColor(128, 128, 128, 0); }
        }

        public static Color LightGrey
        {
          get { return RGBAColor(200, 200, 200, 0); }
        }

        public static Color Transparent
        {
          get { return RGBAColor(255, 255, 255, 0); }
        }

        public static Color AliceBlue
        {
          get { return RGBAColor(240, 248, 255, 255); }
        }

        public static Color AntiqueWhite
        {
          get { return RGBAColor(250, 235, 215, 255); }
        }

        public static Color Aqua
        {
          get { return RGBAColor(0, 255, 255, 255); }
        }

        public static Color Aquamarine
        {
          get { return RGBAColor(127, 255, 212, 255); }
        }

        public static Color Azure
        {
          get { return RGBAColor(240, 255, 255, 255); }
        }

        public static Color Beige
        {
          get { return RGBAColor(245, 245, 220, 255); }
        }

        public static Color Bisque
        {
          get { return RGBAColor(255, 228, 196, 255); }
        }

        public static Color Black
        {
          get { return RGBAColor(0, 0, 0, 255); }
        }

        public static Color BlanchedAlmond
        {
          get { return RGBAColor(255, 235, 205, 255); }
        }

        public static Color Blue
        {
          get { return RGBAColor(0, 0, 255, 255); }
        }

        public static Color BlueViolet
        {
          get { return RGBAColor(138, 43, 226, 255); }
        }

        public static Color Brown
        {
          get { return RGBAColor(165, 42, 42, 255); }
        }

        public static Color BurlyWood
        {
          get { return RGBAColor(222, 184, 135, 255); }
        }

        public static Color CadetBlue
        {
          get { return RGBAColor(95, 158, 160, 255); }
        }

        public static Color Chartreuse
        {
          get { return RGBAColor(127, 255, 0, 255); }
        }

        public static Color Chocolate
        {
          get { return RGBAColor(210, 105, 30, 255); }
        }

        public static Color Coral
        {
          get { return RGBAColor(255, 127, 80, 255); }
        }

        public static Color CornflowerBlue
        {
          get { return RGBAColor(100, 149, 237, 255); }
        }

        public static Color Cornsilk
        {
          get { return RGBAColor(255, 248, 220, 255); }
        }

        public static Color Crimson
        {
          get { return RGBAColor(220, 20, 60, 255); }
        }

        public static Color Cyan
        {
          get { return RGBAColor(0, 255, 255, 255); }
        }

        public static Color DarkBlue
        {
          get { return RGBAColor(0, 0, 139, 255); }
        }

        public static Color DarkCyan
        {
          get { return RGBAColor(0, 139, 139, 255); }
        }

        public static Color DarkGoldenrod
        {
          get { return RGBAColor(184, 134, 11, 255); }
        }

        public static Color DarkGray
        {
          get { return RGBAColor(169, 169, 169, 255); }
        }

        public static Color DarkGreen
        {
          get { return RGBAColor(0, 100, 0, 255); }
        }

        public static Color DarkKhaki
        {
          get { return RGBAColor(189, 183, 107, 255); }
        }

        public static Color DarkMagenta
        {
          get { return RGBAColor(139, 0, 139, 255); }
        }

        public static Color DarkOliveGreen
        {
          get { return RGBAColor(85, 107, 47, 255); }
        }

        public static Color DarkOrange
        {
          get { return RGBAColor(255, 140, 0, 255); }
        }

        public static Color DarkOrchid
        {
          get { return RGBAColor(153, 50, 204, 255); }
        }

        public static Color DarkRed
        {
          get { return RGBAColor(139, 0, 0, 255); }
        }

        public static Color DarkSalmon
        {
          get { return RGBAColor(233, 150, 122, 255); }
        }

        public static Color DarkSeaGreen
        {
          get { return RGBAColor(143, 188, 139, 255); }
        }

        public static Color DarkSlateBlue
        {
          get { return RGBAColor(72, 61, 139, 255); }
        }

        public static Color DarkSlateGray
        {
          get { return RGBAColor(47, 79, 79, 255); }
        }

        public static Color DarkTurquoise
        {
          get { return RGBAColor(0, 206, 209, 255); }
        }

        public static Color DarkViolet
        {
          get { return RGBAColor(148, 0, 211, 255); }
        }

        public static Color DeepPink
        {
          get { return RGBAColor(255, 20, 147, 255); }
        }

        public static Color DeepSkyBlue
        {
          get { return RGBAColor(0, 191, 255, 255); }
        }

        public static Color DimGray
        {
          get { return RGBAColor(105, 105, 105, 255); }
        }

        public static Color DodgerBlue
        {
          get { return RGBAColor(30, 144, 255, 255); }
        }

        public static Color Firebrick
        {
          get { return RGBAColor(178, 34, 34, 255); }
        }

        public static Color FloralWhite
        {
          get { return RGBAColor(255, 250, 240, 255); }
        }

        public static Color ForestGreen
        {
          get { return RGBAColor(34, 139, 34, 255); }
        }

        public static Color Fuchsia
        {
          get { return RGBAColor(255, 0, 255, 255); }
        }

        public static Color Gainsboro
        {
          get { return RGBAColor(220, 220, 220, 255); }
        }

        public static Color GhostWhite
        {
          get { return RGBAColor(248, 248, 255, 255); }
        }

        public static Color Gold
        {
          get { return RGBAColor(255, 215, 0, 255); }
        }

        public static Color Goldenrod
        {
          get { return RGBAColor(218, 165, 32, 255); }
        }

        public static Color Gray
        {
          get { return RGBAColor(128, 128, 128, 255); }
        }

        public static Color Green
        {
          get { return RGBAColor(0, 128, 0, 255); }
        }

        public static Color GreenYellow
        {
          get { return RGBAColor(173, 255, 47, 255); }
        }

        public static Color Honeydew
        {
          get { return RGBAColor(240, 255, 240, 255); }
        }

        public static Color HotPink
        {
          get { return RGBAColor(255, 105, 180, 255); }
        }

        public static Color IndianRed
        {
          get { return RGBAColor(205, 92, 92, 255); }
        }

        public static Color Indigo
        {
          get { return RGBAColor(75, 0, 130, 255); }
        }

        public static Color Ivory
        {
          get { return RGBAColor(255, 255, 240, 255); }
        }

        public static Color Khaki
        {
          get { return RGBAColor(240, 230, 140, 255); }
        }

        public static Color Lavender
        {
          get { return RGBAColor(230, 230, 250, 255); }
        }

        public static Color LavenderBlush
        {
          get { return RGBAColor(255, 240, 245, 255); }
        }

        public static Color LawnGreen
        {
          get { return RGBAColor(124, 252, 0, 255); }
        }

        public static Color LemonChiffon
        {
          get { return RGBAColor(255, 250, 205, 255); }
        }

        public static Color LightBlue
        {
          get { return RGBAColor(173, 216, 230, 255); }
        }

        public static Color LightCoral
        {
          get { return RGBAColor(240, 128, 128, 255); }
        }

        public static Color LightCyan
        {
          get { return RGBAColor(224, 255, 255, 255); }
        }

        public static Color LightGoldenrodYellow
        {
          get { return RGBAColor(250, 250, 210, 255); }
        }

        public static Color LightGreen
        {
          get { return RGBAColor(144, 238, 144, 255); }
        }

        public static Color LightGray
        {
          get { return RGBAColor(211, 211, 211, 255); }
        }

        public static Color LightPink
        {
          get { return RGBAColor(255, 182, 193, 255); }
        }

        public static Color LightSalmon
        {
          get { return RGBAColor(255, 160, 122, 255); }
        }

        public static Color LightSeaGreen
        {
          get { return RGBAColor(32, 178, 170, 255); }
        }

        public static Color LightSkyBlue
        {
          get { return RGBAColor(135, 206, 250, 255); }
        }

        public static Color LightSlateGray
        {
          get { return RGBAColor(119, 136, 153, 255); }
        }

        public static Color LightSteelBlue
        {
          get { return RGBAColor(176, 196, 222, 255); }
        }

        public static Color LightYellow
        {
          get { return RGBAColor(255, 255, 224, 255); }
        }

        public static Color Lime
        {
          get { return RGBAColor(0, 255, 0, 255); }
        }

        public static Color LimeGreen
        {
          get { return RGBAColor(50, 205, 50, 255); }
        }

        public static Color Linen
        {
          get { return RGBAColor(250, 240, 230, 255); }
        }

        public static Color Magenta
        {
          get { return RGBAColor(255, 0, 255, 255); }
        }

        public static Color Maroon
        {
          get { return RGBAColor(128, 0, 0, 255); }
        }

        public static Color MediumAquamarine
        {
          get { return RGBAColor(102, 205, 170, 255); }
        }

        public static Color MediumBlue
        {
          get { return RGBAColor(0, 0, 205, 255); }
        }

        public static Color MediumOrchid
        {
          get { return RGBAColor(186, 85, 211, 255); }
        }

        public static Color MediumPurple
        {
          get { return RGBAColor(147, 112, 219, 255); }
        }

        public static Color MediumSeaGreen
        {
          get { return RGBAColor(60, 179, 113, 255); }
        }

        public static Color MediumSlateBlue
        {
          get { return RGBAColor(123, 104, 238, 255); }
        }

        public static Color MediumSpringGreen
        {
          get { return RGBAColor(0, 250, 154, 255); }
        }

        public static Color MediumTurquoise
        {
          get { return RGBAColor(72, 209, 204, 255); }
        }

        public static Color MediumVioletRed
        {
          get { return RGBAColor(199, 21, 133, 255); }
        }

        public static Color MidnightBlue
        {
          get { return RGBAColor(25, 25, 112, 255); }
        }

        public static Color MintCream
        {
          get { return RGBAColor(245, 255, 250, 255); }
        }

        public static Color MistyRose
        {
          get { return RGBAColor(255, 228, 225, 255); }
        }

        public static Color Moccasin
        {
          get { return RGBAColor(255, 228, 181, 255); }
        }

        public static Color NavajoWhite
        {
          get { return RGBAColor(255, 222, 173, 255); }
        }

        public static Color Navy
        {
          get { return RGBAColor(0, 0, 128, 255); }
        }

        public static Color OldLace
        {
          get { return RGBAColor(253, 245, 230, 255); }
        }

        public static Color Olive
        {
          get { return RGBAColor(128, 128, 0, 255); }
        }

        public static Color OliveDrab
        {
          get { return RGBAColor(107, 142, 35, 255); }
        }

        public static Color Orange
        {
          get { return RGBAColor(255, 165, 0, 255); }
        }

        public static Color OrangeRed
        {
          get { return RGBAColor(255, 69, 0, 255); }
        }

        public static Color Orchid
        {
          get { return RGBAColor(218, 112, 214, 255); }
        }

        public static Color PaleGoldenrod
        {
          get { return RGBAColor(238, 232, 170, 255); }
        }

        public static Color PaleGreen
        {
          get { return RGBAColor(152, 251, 152, 255); }
        }

        public static Color PaleTurquoise
        {
          get { return RGBAColor(175, 238, 238, 255); }
        }

        public static Color PaleVioletRed
        {
          get { return RGBAColor(219, 112, 147, 255); }
        }

        public static Color PapayaWhip
        {
          get { return RGBAColor(255, 239, 213, 255); }
        }

        public static Color PeachPuff
        {
          get { return RGBAColor(255, 218, 185, 255); }
        }

        public static Color Peru
        {
          get { return RGBAColor(205, 133, 63, 255); }
        }

        public static Color Pink
        {
          get { return RGBAColor(255, 192, 203, 255); }
        }

        public static Color Plum
        {
          get { return RGBAColor(221, 160, 221, 255); }
        }

        public static Color PowderBlue
        {
          get { return RGBAColor(176, 224, 230, 255); }
        }

        public static Color Purple
        {
          get { return RGBAColor(128, 0, 128, 255); }
        }

        public static Color Red
        {
          get { return RGBAColor(255, 0, 0, 255); }
        }

        public static Color RosyBrown
        {
          get { return RGBAColor(188, 143, 143, 255); }
        }

        public static Color RoyalBlue
        {
          get { return RGBAColor(65, 105, 225, 255); }
        }

        public static Color SaddleBrown
        {
          get { return RGBAColor(139, 69, 19, 255); }
        }

        public static Color Salmon
        {
          get { return RGBAColor(250, 128, 114, 255); }
        }

        public static Color SandyBrown
        {
          get { return RGBAColor(244, 164, 96, 255); }
        }

        public static Color SeaGreen
        {
          get { return RGBAColor(46, 139, 87, 255); }
        }

        public static Color SeaShell
        {
          get { return RGBAColor(255, 245, 238, 255); }
        }

        public static Color Sienna
        {
          get { return RGBAColor(160, 82, 45, 255); }
        }

        public static Color Silver
        {
          get { return RGBAColor(192, 192, 192, 255); }
        }

        public static Color SkyBlue
        {
          get { return RGBAColor(135, 206, 235, 255); }
        }

        public static Color SlateBlue
        {
          get { return RGBAColor(106, 90, 205, 255); }
        }

        public static Color SlateGray
        {
          get { return RGBAColor(112, 128, 144, 255); }
        }

        public static Color Snow
        {
          get { return RGBAColor(255, 250, 250, 255); }
        }

        public static Color SpringGreen
        {
          get { return RGBAColor(0, 255, 127, 255); }
        }

        public static Color SteelBlue
        {
          get { return RGBAColor(70, 130, 180, 255); }
        }

        public static Color Tan
        {
          get { return RGBAColor(210, 180, 140, 255); }
        }

        public static Color Teal
        {
          get { return RGBAColor(0, 128, 128, 255); }
        }

        public static Color Thistle
        {
          get { return RGBAColor(216, 191, 216, 255); }
        }

        public static Color Tomato
        {
          get { return RGBAColor(255, 99, 71, 255); }
        }

        public static Color Turquoise
        {
          get { return RGBAColor(64, 224, 208, 255); }
        }

        public static Color Violet
        {
          get { return RGBAColor(238, 130, 238, 255); }
        }

        public static Color Wheat
        {
          get { return RGBAColor(245, 222, 179, 255); }
        }

        public static Color White
        {
          get { return RGBAColor(255, 255, 255, 255); }
        }

        public static Color WhiteSmoke
        {
          get { return RGBAColor(245, 245, 245, 255); }
        }

        public static Color Yellow
        {
          get { return RGBAColor(255, 255, 0, 255); }
        }

        public static Color YellowGreen
        {
          get { return RGBAColor(154, 205, 50, 255); }
        }

    }
}