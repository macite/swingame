program OpenGLTextTest;
uses
  sgDriverGraphics, sgGraphics, sgUtils,sgInput, sgDriverTextOpenGL, 
  math, gl, glext, glu, FreeType, SwinGame, sgTypes;
  
// All code copyright to Ryan Pridgeon, 2009
// If you wish to use this code, you must include these credits
// in your code, and give credit in your application/documentation
// Thankyou
//

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}




procedure Main();
var
	f: GLFont;
    f1: Font;
    m:  array [0..15] of single;
    glsl_code, msg: String;
    myFragShader, myProg : GLuint;
    src:    PChar;
    len, compiled : glInt;
    temp: gluint;
    success: glInt;
begin
    OpenAudio();
    glsl_code :=
        ' uniform sampler2D Tex_Clr0; '#10 +
        ' uniform sampler2D Tex_Alpha0; '#10 +
        ' void main(void) { //vec4 OutColor(0,0,0,0); '#10 +
        ' vec4 myColor = texture2D(Tex_Clr0,gl_TexCoord[0].xy) * gl_Color; '#10 +
        ' //myColor.a = texture2D(Tex_Alpha0,gl_TexCoord[1].xy); '#10 +
        ' gl_FragColor = myColor; '#10 +
        '} ';
    
    WriteLn('Face ', sizeof(FT_FaceRec));
    WriteLn('Glyph ', sizeof(FT_GlyphRec));

	Writeln('opening window');
	OpenGraphicsWindow('Hello World', 640, 480);
    LoadDefaultColors();
    //InitGL();
	Writeln('clearing screen to red');
    // 
    // glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
    ClearScreen(2147483647);
	Writeln('loading font');
    
    // writeln('here');
    // myFragShader := glCreateShader(GL_FRAGMENT_SHADER);
    // // srcAddr := @(glsl_code[0]);
    // len := Length(glsl_code);
    // src := PChar(glsl_code);
    // writeln('here');
    // glShaderSource(myFragShader, 1, @src, @len);
    // writeln('here');
    // glCompileShader(myFragShader);
    // writeln('here');
    // glGetShaderiv(myFragShader, GL_COMPILE_STATUS, @compiled);
    // msg := ShaderGetInfoLog(myFragShader);
    // if compiled <> GL_TRUE then
    // begin
    //   WriteLn('Error compiling shader : ', msg);
    //   exit;
    // end;

    // myProg := glCreateProgram();
    // glAttachShader(myProg, myFragShader);
    // glLinkProgram(myProg);
    // glGetProgramIV(myProg, GL_LINK_STATUS, @success);
    // if success = GL_FALSE then
    // begin
    //     WriteLn('GL link failed');
    //     exit;
    // end;

    // glValidateProgram(myProg);
    // glGetProgramIV(myProg, GL_VALIDATE_STATUS, @success);
    // if success = GL_FALSE then
    // begin
    //     WriteLn('GL Validate failed');
    //     exit;
    // end;

    f := sgDriverTextOpenGL.LoadFont('/Users/acain/working/SwinGameSDK/CoreSDK/test/Resources/fonts/arial.ttf', 18);
    
	WriteLn('Loaded font...', HexStr(f));


    // glUseProgram(myProg);
    glColor4f ( 1.0, 0, 0, 1.0 );
	 
   repeat
   ProcessEvents();
   ClearScreen(ColorWhite);
   PrintFont(f, 'abcdefghijklmnopqrstuvwxyz', 50, 50);
    // glUseProgram(0);

    PrintFont(f, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 55, 55);

    
    f1 := LoadFont('arial.ttf', 18);
    DrawText('TL', ColorBlack, f1, 0, 0);
    DrawText('TR', ColorBlack, f1, ScreenWidth() - TextWidth(f1, 'TR'), 0);
    DrawText('BL', ColorBlack, f1, 0, ScreenHeight() - TextHeight(f1, 'BL'));
    DrawText('BR', ColorBlack, f1, ScreenWidth() - TextWidth(f1, 'BR'), ScreenHeight() - TextHeight(f1, 'BR'));

    DrawText('1234567890!@#$%^&*()', ColorBlack, f1, 50, 400);
    DrawText('SwinGame API by Swinburne University of Technology', ColorBlue, f1, 50, 450);

    DrawFrameRate(0,0);
    RefreshScreen();

    until WindowCloseRequested();

	//FreeFont(f);

	exit;	


	repeat
		ProcessEvents();
		ClearScreen(rnd(2147483647));
		RefreshScreen();
	until WindowCloseRequested();
end;

begin
	Main();
end.