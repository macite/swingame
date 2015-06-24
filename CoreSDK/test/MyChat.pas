program MyChat;
uses SwinGame, sgTypes, SysUtils;

procedure LoadResources();
begin
	LoadFontNamed('InputFont', 'arial.ttf', 14);
	LoadFontNamed('DialogFont', 'arial.ttf', 14);
end;

procedure DisplayDialog(const msg: String; x,y: Integer); 
var
	msgWidth, promptWidth, msgHeight: Integer;
	width, height: Integer;
	outputFont: Font;
	prompt: String = 'Press enter to continue...';
begin
	outputFont := FontNamed('DialogFont');

	msgWidth := TextWidth(outputFont, msg);
	promptWidth := TextWidth(outputFont, prompt);
	msgHeight := TextHeight(outputFont, msg);

	if msgWidth > promptWidth then
		width := msgWidth + 20 //10px left, right 
	else
		width := promptWidth + 20;

	height := msgHeight * 2 + 30; // 10px above, between + below

	FillRectangle(ColorWhite, x, y, width, height);
	DrawRectangle(ColorBlack, x, y, width, height);
	DrawText(msg, ColorBlack, outputFont, x + 10, y + 10);
	DrawText(prompt, ColorBlack, outputFont, x + 10, y + 20 + msgHeight);
	RefreshScreen();

	repeat
		ProcessEvents();
	until KeyTyped(VK_RETURN) or WindowCloseRequested();
end;

function ReadString(const prompt: String; x, y: Integer): String;
var
	promptHeight: Integer;
	inputAreaWidth, inputAreaHeight: Integer;
	inputFont: Font;
begin
	inputFont := FontNamed('InputFont');
	promptHeight := TextHeight(inputFont, prompt);

	inputAreaHeight := promptHeight * 2 + 30; // 10px above, between, below
	inputAreaWidth := TextWidth(inputFont, StringOfChar('M',30)) + 20; //10px left, right 

	StartReadingText(ColorBlack, 30, inputFont, x + 10, y + promptHeight + 20);

	while ReadingText() and not WindowCloseRequested() do
	begin
		ProcessEvents();

		FillRectangle(ColorWhite, x, y, inputAreaWidth, inputAreaHeight);
		DrawRectangle(ColorBlack, x, y, inputAreaWidth, inputAreaHeight);
		DrawRectangle(ColorBlack, x + 8, y + promptHeight + 18, inputAreaWidth - 16, promptHeight + 4);

		DrawText(prompt, ColorBlack, inputFont, x + 10, y + 10);
		RefreshScreen();
	end;

	result := TextReadAsASCII();
end;

function ReadInteger(const prompt: String; x, y: Integer): Integer;
var
	line: String;
	num: Integer;
begin
	line := ReadString(prompt, x, y);
	while not WindowCloseRequested() and not TryStrToInt(line, num) do
	begin
		DisplayDialog('Please enter a whole number.', x, y);
		line := ReadString(prompt, x, y);
	end;
	result := num;
end;

function ReadIntegerRange(const prompt: String; min, max, x, y: Integer): Integer;
var
	errorMsg: String;
begin
	result := ReadInteger(prompt, x, y);
	while not WindowCloseRequested() and ((result < min) or (result > max)) do
	begin
		errorMsg := 'Please enter a number between ' + IntToStr(min) + 
					' and ' + IntToStr(max);

		DisplayDialog(errorMsg, x, y);
		result := ReadInteger(prompt, x, y);
	end;
end;

procedure Main();
var
	answer: String;
	i: Integer;
begin
	OpenGraphicsWindow('My Chat', 600, 600);
	LoadDefaultColors();
	LoadResources();

	ClearScreen(ColorWhite);
	answer := ReadString('Do you want to be a server?', 100, 100);

	if answer = 'yes' then
	begin
		if CreateServer('MyChat', 50000) = nil then
		begin
			DisplayDialog('Server failed to start.', 100, 100);
			exit;
		end;
	end
	else
	begin
		if OpenConnection('ToServer', ReadString('Enter host address.', 100, 100), 50000) = nil then
		begin
			DisplayDialog('Unable to connect to that server.', 100, 100);
			exit;
		end;

		SendMessageTo('Hello Server', 'ToServer');
	end;

	repeat
		ProcessEvents();
		CheckNetworkActivity();
		ClearScreen(ColorWhite);

		while HasMessages() do
		begin
			if answer = 'yes' then
			begin
				WriteLn(ReadMessageData('MyChat'));
			end;
		end;

		RefreshScreen(60);
	until WindowCloseRequested();
end;

begin
	Main();
end.

