program TestHttp;
uses SwinGame, 
	sgNetworking;

procedure Main();
begin
	// OpenGraphicsWindow('Testing HTTP', 800, 600);

	// WriteLn(HostIP('swin.edu.au'));
	// WriteLn(HostName('127.0.0.1'));

	// WriteLn( HttpResponseBodyAsString( HttpGet('wiki.freepascal.org', 80, '/cardinal') ) );

	WriteLn( HttpResponseBodyAsString( HttpGet('www.swinburne.edu.au', 80, '/fred') ) );
end;

begin
	Main();
end.