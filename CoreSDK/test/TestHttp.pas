program TestHttp;
uses sgNetworking, fpjson, jsonparser;

procedure Main();
var
	json, token: String;
	j: TJSONData;
begin
	// OpenGraphicsWindow('Testing HTTP', 800, 600);

	// WriteLn(HostIP('swin.edu.au'));
	// WriteLn(HostName('127.0.0.1'));

	// WriteLn( HttpResponseBodyAsString( HttpGet('wiki.freepascal.org', 80, '/cardinal') ) );

	json := HttpResponseBodyAsString( HttpPost('localhost', 3000, '/api/auth', '{"username":"acain","password":"password","remember":true}'));
	WriteLn(json);
	j := GetJSON(json);
	token := j.FindPath('auth_token').AsString;
	WriteLn(token);
	j.destroy();

	json := HttpResponseBodyAsString( HttpGet('localhost', 3000, '/api/units.json?auth_token=' + token));

	WriteLn(json);

	json := HttpResponseBodyAsString( HttpGet('api.twitter.com', 80, '/1.1/statuses/user_timeline.json'));
	WriteLn(json);

	// WriteLn( HttpResponseBodyAsString( HttpGet('www.swinburne.edu.au', 80, '/fred') ) );
end;

begin
	Main();
end.