program TestHttp;
uses sgNetworking, fpjson, jsonparser;

procedure Main();
var
	json: String;
	j: TJSONData;
begin
	// OpenGraphicsWindow('Testing HTTP', 800, 600);

	// WriteLn(HostIP('swin.edu.au'));
	// WriteLn(HostName('127.0.0.1'));

	// WriteLn( HttpResponseBodyAsString( HttpGet('wiki.freepascal.org', 80, '/cardinal') ) );

	json := HttpResponseBodyAsString( HttpGet('doubtfire.ict.swin.edu.au', 80, '/api/units.json'));

	WriteLn(json);

	j := GetJSON(json);

	WriteLn('here');

	WriteLn(j.FindPath('error').AsString);

	j.destroy();

	// WriteLn( HttpResponseBodyAsString( HttpGet('www.swinburne.edu.au', 80, '/fred') ) );
end;

begin
	Main();
end.