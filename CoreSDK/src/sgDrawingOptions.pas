unit sgDrawingOptions;

interface
uses sgTypes;

function OptionDefaults(): BitmapDrawOpts;

function OptionDrawTo(dest: Bitmap): BitmapDrawOpts;

function OptionDrawTo(dest: Bitmap; const opts: BitmapDrawOpts): BitmapDrawOpts;

function OptionScaleBmp(scaleX, scaleY: Single): BitmapDrawOpts;

function OptionScaleBmp(scaleX, scaleY: Single; const opts: BitmapDrawOpts): BitmapDrawOpts;

function OptionRotateBmp(angle, anchorX, anchorY : Single) : BitmapDrawOpts;

function OptionRotateBmp(angle, anchorX, anchorY : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;

function OptionRotateBmp(angle : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;

function OptionRotateBmp(angle : Single) : BitmapDrawOpts;

function OptionFlipX() : BitmapDrawOpts;

function OptionFlipX(const opts : BitmapDrawOpts) : BitmapDrawOpts;

function OptionFlipY() : BitmapDrawOpts;

function OptionFlipY(const opts : BitmapDrawOpts) : BitmapDrawOpts;

function OptionFlipXY() : BitmapDrawOpts;

function OptionFlipXY(const opts : BitmapDrawOpts) : BitmapDrawOpts;

function OptionPartBmp(x, y, w, h : Single) : BitmapDrawOpts;

function OptionPartBmp(x, y, w, h : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;

implementation
uses sgShared;

function OptionDefaults(): BitmapDrawOpts;
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
	end;
end;

function OptionDrawTo(dest: Bitmap): BitmapDrawOpts;
begin
	result := OptionDrawTo(dest, OptionDefaults());
end;

function OptionDrawTo(dest: Bitmap; const opts: BitmapDrawOpts): BitmapDrawOpts;
begin
	result := opts;
	result.dest := dest;
end;

function OptionScaleBmp(scaleX, scaleY: Single): BitmapDrawOpts;
begin
	result := OptionScaleBmp(scaleX, scaleY, OptionDefaults());
end;

function OptionScaleBmp(scaleX, scaleY: Single; const opts: BitmapDrawOpts): BitmapDrawOpts;
begin
	result := opts;
	result.scaleX := opts.scaleX * scaleX;
	result.scaleY := opts.scaleY * scaleY;
end;

function OptionRotateBmp(angle, anchorX, anchorY : Single) : BitmapDrawOpts;
begin
	result := OptionRotateBmp(angle, anchorX, anchorY, OptionDefaults());
end;

function OptionRotateBmp(angle, anchorX, anchorY : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := opts;
	result.angle += angle;
	result.anchoroffsetX := anchorX;
	result.anchoroffsetY := anchorY;
end;

function OptionRotateBmp(angle : Single) : BitmapDrawOpts;
begin
	result := OptionRotateBmp(angle, 0, 0, OptionDefaults());
end;

function OptionRotateBmp(angle : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := OptionRotateBmp(angle, 0, 0, opts);
end;

function OptionFlipX() : BitmapDrawOpts;
begin
	result := OptionFlipX(OptionDefaults());
end;

function OptionFlipX(const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := opts;
	result.flipX := not result.flipX;
end;

function OptionFlipY() : BitmapDrawOpts;
begin
	result := OptionFlipY(OptionDefaults());
end;

function OptionFlipY(const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := opts;
	result.flipY := not result.flipY;
end;

function OptionFlipXY() : BitmapDrawOpts;
begin
	result := OptionFlipXY(OptionDefaults());
end;

function OptionFlipXY(const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := opts;
	result.flipY := not result.flipY;
	result.flipX := not result.flipX;
end;

function OptionPartBmp(x, y, w, h : Single) : BitmapDrawOpts;
begin
	result := OptionPartBmp(x, y, w, h, OptionDefaults());
end;

function OptionPartBmp(x, y, w, h : Single; const opts : BitmapDrawOpts) : BitmapDrawOpts;
begin
	result := opts;
	result.isPart := true;
	result.x := x;
	result.y := y;
	result.w := w;
	result.h := h;
end;

end.