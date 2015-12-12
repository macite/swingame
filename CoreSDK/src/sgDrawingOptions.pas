//=============================================================================
// sgDrawingOptions.pas
//=============================================================================
//
// The drawing options provides a flexible means of providing different drawing
// features, without needing many different overloads of each function or 
// procedure.
//
// Drawing Options can be composed to provide a range of different features
// including:
// - Drawing onto another Bitmap, rather than the screen
// - Scaling the drawing
// - Rotation
// - Flipping X and/or Y
// - Drawing part of a bitmap
// - Drawing to the World or Screen
//

/// DrawingOptions allows you to provide various options (such as scaling, 
/// rotation, and drawing on to a bitmap) for SwinGame's drawing operations.
///
/// The one DrawingOption value can contain a number of different options.
///
/// @module DrawingOptionsConfiguration
/// @static
///
/// @doc_types DrawingOptions
unit sgDrawingOptions;

interface
uses sgTypes;

/// Returns a DrawingOptions with default values. 
///
/// @lib
function OptionDefaults(): DrawingOptions;

/// Use this option to draw to a Bitmap. Pass dest the Bitmap you want to draw on.
///
/// @lib OptionDrawToBitmap
function OptionDrawTo(dest: Bitmap): DrawingOptions;

/// Use this option to draw to a Bitmap. Pass dest the Bitmap you want to draw on.
/// Pass opts the other options you want use.
///
/// @lib OptionDrawToBitmapOpts
/// @sn optionDrawToBitmap:%s withOptions:%s
function OptionDrawTo(dest: Bitmap; const opts: DrawingOptions): DrawingOptions;

/// Use this option to draw to a specified Window. Pass dest the Window you want to draw on.
///
/// @lib OptionDrawToWindow
function OptionDrawTo(dest: Window): DrawingOptions;

/// Use this option to draw to a Bitmap. Pass dest the Bitmap you want to draw on to.
/// Pass opts the other options you want use.
///
/// @lib OptionDrawToWindowOpts
/// @sn optionDrawToWindow:%s withOptions:%s
function OptionDrawTo(dest: Window; const opts: DrawingOptions): DrawingOptions;


/// Use this option to scale the drawing of bitmaps. You can scale x and y separately.
///
/// @lib
function OptionScaleBmp(scaleX, scaleY: Single): DrawingOptions;

/// Use this option to scale the drawing of bitmaps. You can scale x and y separately.
/// Pass opts the other options you want use.
///
/// @lib OptionScaleBmpOpts
function OptionScaleBmp(scaleX, scaleY: Single; const opts: DrawingOptions): DrawingOptions;

/// Use this option to rotate the drawing of a bitmap. This allows you to set the
/// anchor point and rotate around that by a number of degrees.
///
/// @lib
function OptionRotateBmp(angle, anchorX, anchorY: Single): DrawingOptions;

/// Use this option to rotate the drawing of a bitmap. This allows you to set the
/// anchor point and rotate around that by a number of degrees.
/// Pass opts the other options you want use.
///
/// @lib OptionRotateBmpOpts
function OptionRotateBmp(angle, anchorX, anchorY: Single; const opts: DrawingOptions): DrawingOptions;

/// Use this option to rotate a bitmap around its centre point.
///
/// @lib OptionRotateBmpAngleOpts
function OptionRotateBmp(angle: Single; const opts: DrawingOptions): DrawingOptions;

/// Use this option to rotate a bitmap around its centre point.
/// Pass opts the other options you want use.
///
/// @lib OptionRotateBmpAngle
function OptionRotateBmp(angle: Single): DrawingOptions;

/// Use this option to flip an image along its X axis.
///
/// @lib
function OptionFlipX(): DrawingOptions;

/// Use this option to flip an image along its X axis.
/// Pass opts the other options you want use.
///
/// @lib OptionFlipXOpts
function OptionFlipX(const opts: DrawingOptions): DrawingOptions;

/// Use this option to flip the drawing of an image along its Y axis.
///
/// @lib
function OptionFlipY(): DrawingOptions;

/// Use this option to flip the drawing of an image along its Y axis.
/// Pass opts the other options you want use.
///
/// @lib OptionFlipYOpts
function OptionFlipY(const opts: DrawingOptions): DrawingOptions;

/// Use this option to flow the drawing of an image along both X and Y axis.
///
/// @lib
function OptionFlipXY(): DrawingOptions;

/// Use this option to flow the drawing of an image along both X and Y axis.
/// Pass opts the other options you want use.
///
/// @lib OptionFlipXYOpts
function OptionFlipXY(const opts: DrawingOptions): DrawingOptions;

/// Use this option to draw only a part of a bitmap.
///
/// @lib
function OptionPartBmp(x, y, w, h: Single): DrawingOptions;

/// Use this option to draw only a part of a bitmap.
/// Pass opts the other options you want use.
///
/// @lib OptionPartBmpOpts
function OptionPartBmp(x, y, w, h: Single; const opts: DrawingOptions): DrawingOptions;

/// Use this option to draw only part of a bitmap.
///
/// @lib OptionPartBmpRect
function OptionPartBmp(const part: Rectangle): DrawingOptions;

/// Use this option to draw only part of a bitmap.
/// Pass opts the other options you want use.
///
/// @lib OptionPartBmpRectOpts
function OptionPartBmp(const part: Rectangle; const opts: DrawingOptions): DrawingOptions;

/// Use this option to draw in World coordinates -- these are affected by the movement of the camera.
///
/// @lib
function OptionToWorld(): DrawingOptions;

/// Use this option to draw in World coordinates -- these are affected by the movement of the camera.
/// Pass opts the other options you want use.
///
/// @lib OptionToWorldOpts
function OptionToWorld(const opts: DrawingOptions): DrawingOptions;

/// Use this option to draw to the screen, ignoring the positon of the camera.
///
/// @lib
function OptionToScreen(): DrawingOptions;

/// Use this option to draw to the screen, ignoring the positon of the camera.
/// Pass opts the other options you want use.
///
/// @lib OptionToScreenOpts
function OptionToScreen(const opts: DrawingOptions): DrawingOptions;

/// Use this option to change the width of line drawings.
///
/// @lib
function OptionLineWidth(width: Longint): DrawingOptions;

/// Use this option to change the width of line drawings.
///
/// @lib OptionLineWidthOpts
function OptionLineWidth(width: Longint; const opts: DrawingOptions): DrawingOptions;

implementation
uses sgShared, sgGeometry;

function OptionDefaults(): DrawingOptions;
begin
	with result do
	begin
		dest 			:= _CurrentWindow;
		scaleX 			:= 1;
		scaleY 			:= 1;
		angle  			:= 0;
		anchorOffsetX 	:= 0;
		anchorOffsetY 	:= 0;
		flipX 			:= false;
		flipY 			:= false;
		isPart 			:= false;
		part 			:= RectangleFrom(0,0,1,1);
		camera	 		:= DrawDefault;
		lineWidth 		:= 1;
	end;
end;

function OptionDrawTo(dest: Bitmap): DrawingOptions;
begin
	result := OptionDrawTo(dest, OptionDefaults());
end;

function OptionDrawTo(dest: Bitmap; const opts: DrawingOptions): DrawingOptions;
begin
	result := opts;
	result.dest := dest;
end;

function OptionDrawTo(dest: Window): DrawingOptions;
begin
	result := OptionDrawTo(dest, OptionDefaults());
end;

function OptionDrawTo(dest: Window; const opts: DrawingOptions): DrawingOptions;
begin
	result := opts;
	result.dest := dest;
end;

function OptionLineWidth(width: Longint): DrawingOptions;
begin
	result := OptionLineWidth(width, OptionDefaults());
end;

function OptionLineWidth(width: Longint; const opts: DrawingOptions): DrawingOptions;
begin
	result := opts;
	result.lineWidth := width;
end;

function OptionScaleBmp(scaleX, scaleY: Single): DrawingOptions;
begin
	result := OptionScaleBmp(scaleX, scaleY, OptionDefaults());
end;

function OptionScaleBmp(scaleX, scaleY: Single; const opts: DrawingOptions): DrawingOptions;
begin
	result := opts;
	result.scaleX := opts.scaleX * scaleX;
	result.scaleY := opts.scaleY * scaleY;
end;

function OptionRotateBmp(angle, anchorX, anchorY : Single) : DrawingOptions;
begin
	result := OptionRotateBmp(angle, anchorX, anchorY, OptionDefaults());
end;

function OptionRotateBmp(angle, anchorX, anchorY : Single; const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.angle += angle;
	result.anchoroffsetX := anchorX;
	result.anchoroffsetY := anchorY;
end;

function OptionRotateBmp(angle : Single) : DrawingOptions;
begin
	result := OptionRotateBmp(angle, 0, 0, OptionDefaults());
end;

function OptionRotateBmp(angle : Single; const opts : DrawingOptions) : DrawingOptions;
begin
	result := OptionRotateBmp(angle, 0, 0, opts);
end;

function OptionFlipX() : DrawingOptions;
begin
	result := OptionFlipX(OptionDefaults());
end;

function OptionFlipX(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.flipX := not result.flipX;
end;

function OptionFlipY() : DrawingOptions;
begin
	result := OptionFlipY(OptionDefaults());
end;

function OptionFlipY(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.flipY := not result.flipY;
end;

function OptionFlipXY() : DrawingOptions;
begin
	result := OptionFlipXY(OptionDefaults());
end;

function OptionFlipXY(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.flipY := not result.flipY;
	result.flipX := not result.flipX;
end;

function OptionPartBmp(x, y, w, h : Single) : DrawingOptions;
begin
	result := OptionPartBmp(x, y, w, h, OptionDefaults());
end;

function OptionPartBmp(x, y, w, h : Single; const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.isPart := true;
	result.part.x := x;
	result.part.y := y;
	result.part.width := w;
	result.part.height := h;
end;

function OptionPartBmp(const part: Rectangle): DrawingOptions;
begin
	result := OptionPartBmp(part, OptionDefaults());
end;

function OptionPartBmp(const part: Rectangle; const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.isPart := true;
	result.part := part;
end;


function OptionToWorld() : DrawingOptions;
begin
	result := OptionToWorld(OptionDefaults());
end;

function OptionToWorld(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.camera := DrawToWorld;
end;

function OptionToScreen() : DrawingOptions;
begin
	result := OptionToScreen(OptionDefaults());
end;

function OptionToScreen(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.camera := DrawToScreen;
end;

end.