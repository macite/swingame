Procedure LoadDefaultColors();

var
      ColorWhite :        Color = $FFFFFFFF;
      ColorGreen :        Color = $FF00FF00;
      ColorBlue :         Color = $FF0000FF;
      ColorBlack :        Color = $FF000000;
      ColorRed :          Color = $FFFF0000;
      ColorYellow :       Color = $FFFFFF00;
      ColorPink :         Color = $FFFF1493;
      ColorTurquoise :    Color = $FF00CED1;
      ColorGrey :         Color = $FF808080;
      ColorMagenta :      Color = $FF00FFFF;
      ColorTransparent :  Color = $00000000;
      ColorLightGrey :    Color = $FFC8C8C8;

implementation

  procedure LoadDefaultColors();
  begin
      ColorWhite :=         sgGraphics.ColorWhite;
      ColorGreen :=         sgGraphics.ColorGreen;
      ColorBlue :=          sgGraphics.ColorBlue ;
      ColorBlack :=         sgGraphics.ColorBlack;
      ColorRed :=           sgGraphics.ColorRed  ;
      ColorYellow :=        sgGraphics.ColorYellow;
      ColorPink :=          sgGraphics.ColorPink;
      ColorTurquoise :=     sgGraphics.ColorTurquoise;
      ColorGrey :=          sgGraphics.ColorGrey;
      ColorMagenta :=       sgGraphics.ColorMagenta;
      ColorTransparent :=   sgGraphics.ColorTransparent;
      ColorLightGrey :=     sgGraphics.ColorLightGrey;
  end;
    