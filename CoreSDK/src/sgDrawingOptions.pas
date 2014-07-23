unit sgDrawingOptions;

interface
uses sgTypes;

function OptionDefaults(): DrawingOptions;

function OptionDrawTo(dest: Bitmap): DrawingOptions;

function OptionDrawTo(dest: Bitmap; const opts: DrawingOptions): DrawingOptions;

function OptionScaleBmp(scaleX, scaleY: Single): DrawingOptions;

function OptionScaleBmp(scaleX, scaleY: Single; const opts: DrawingOptions): DrawingOptions;

function OptionRotateBmp(angle, anchorX, anchorY : Single) : DrawingOptions;

function OptionRotateBmp(angle, anchorX, anchorY : Single; const opts : DrawingOptions) : DrawingOptions;

function OptionRotateBmp(angle : Single; const opts : DrawingOptions) : DrawingOptions;

function OptionRotateBmp(angle : Single) : DrawingOptions;

function OptionFlipX() : DrawingOptions;

function OptionFlipX(const opts : DrawingOptions) : DrawingOptions;

function OptionFlipY() : DrawingOptions;

function OptionFlipY(const opts : DrawingOptions) : DrawingOptions;

function OptionFlipXY() : DrawingOptions;

function OptionFlipXY(const opts : DrawingOptions) : DrawingOptions;

function OptionPartBmp(x, y, w, h : Single) : DrawingOptions;

function OptionPartBmp(x, y, w, h : Single; const opts : DrawingOptions) : DrawingOptions;

function OptionToWorld() : DrawingOptions;

function OptionToWorld(const opts : DrawingOptions) : DrawingOptions;

function OptionToScreen() : DrawingOptions;

function OptionToScreen(const opts : DrawingOptions) : DrawingOptions;

implementation
uses sgShared;

function OptionDefaults(): DrawingOptions;
begin
	with result do
	begin
		dest := screen;
		scaleX := 1;
		scaleY := 1;
		angle := 0;
		anchoroffsetX := 0;
		anchoroffsetY := 0;
		flipX := false;
		flipY := false;
		isPart := false;
		ToWorld := false;
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
	result.x := x;
	result.y := y;
	result.w := w;
	result.h := h;
end;

function OptionToWorld() : DrawingOptions;
begin
	result := OptionToWorld(OptionDefaults());
end;

function OptionToWorld(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.ToWorld := true;
end;

function OptionToScreen() : DrawingOptions;
begin
	result := OptionToScreen(OptionDefaults());
end;

function OptionToScreen(const opts : DrawingOptions) : DrawingOptions;
begin
	result := opts;
	result.ToWorld := false;
end;

end.