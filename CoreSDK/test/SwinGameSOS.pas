program SwinGameSOS;
uses sgGraphics, sgUtils;

procedure ShortSignal();
begin
  ClearScreen(ColorWhite);
  RefreshScreen();
  Delay(100);
  ClearScreen(ColorBlack);
  RefreshScreen();
  Delay(100);
end;

procedure LongSignal();
begin
  ClearScreen(ColorWhite);
  RefreshScreen();
  Delay(300);
  ClearScreen(ColorBlack);
  RefreshScreen();
  Delay(300);
end;

procedure SignalO();
begin
  Delay(500);
  LongSignal();
  LongSignal();
  LongSignal();
end;


procedure SignalS();
begin
  Delay(500);
  ShortSignal();
  ShortSignal();
  ShortSignal();
end;

begin
  OpenGraphicsWindow('SwinGame SOS', 800, 600);
  SignalS();
  SignalO();
  SignalS();
end.