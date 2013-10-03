program ArduinoTest;
uses SwinGame, sgTypes;

procedure Main();
var
	dev: ArduinoDevice;
	i: Integer;
begin
	dev := CreateArduinoDevice('/dev/tty.usbmodem1d21', 9600);

	i := 0;
	while i < 10 do
	begin
		repeat
			Delay(20);
			Write('.');
		until ArduinoHasData(dev);
		WriteLn();
		WriteLn(ArduinoReadLine(dev));
		i += 1;
	end;

	FreeArduinoDevice(dev);
end;

begin
	Main();
end.