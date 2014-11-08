program TestHttp;
uses SwinGame, 
	sgNetworking;

procedure Main();
begin
	// OpenGraphicsWindow('Testing HTTP', 800, 600);

	WriteLn(HostIP('swin.edu.au'));
	WriteLn(HostName('127.0.0.1'));

	WriteLn( HttpGet('wiki.freepascal.org', '/cardinal') );
end;

begin
	Main();
end.