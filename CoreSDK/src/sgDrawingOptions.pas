unit sgDrawingOptions;

interface
uses sgTypes;

function OptionDefaults(): BitmapDrawOpts;

function OptionDrawTo(dest: Bitmap): BitmapDrawOpts;

function OptionDrawTo(dest: Bitmap; const opts: BitmapDrawOpts): BitmapDrawOpts;

function OptionScaleBmp(scaleX, scaleY: single): BitmapDrawOpts;

function OptionScaleBmp(scaleX, scaleY: single; const opts: BitmapDrawOpts): BitmapDrawOpts;

implementation
uses sgShared;

function OptionDefaults(): BitmapDrawOpts;
begin
	with result do
	begin
		dest := screen;
		scaleX := 1;
		scaleY := 1;
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

end.