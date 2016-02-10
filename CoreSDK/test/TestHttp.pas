program TestHttp;
uses SwinGame, fpjson, jsonparser;

procedure Main();
var
	json, token: String;
	j: TJSONData;
begin
	// OpenGraphicsWindow('Testing HTTP', 800, 600);

	// WriteLn(HostIP('swin.edu.au'));
	// WriteLn(HostName('127.0.0.1'));

	WriteLn( HttpResponseBodyAsString( HttpGet('http://wiki.freepascal.org/cardinal', 80) ) );
	WriteLn( HttpResponseBodyAsString( HttpGet('https://google.com', 443) ) );

	json := HttpResponseBodyAsString( HttpPost('http://localhost/api/auth', 3000, '{"username":"acain","password":"password","remember":true}'));
	WriteLn(1, json);
	j := GetJSON(json);
	token := j.FindPath('auth_token').AsString;
	WriteLn(2, token);
	j.destroy();

	json := HttpResponseBodyAsString( HttpGet('http://localhost/api/units.json?auth_token=' + token, 3000) );

	WriteLn(3, json);

	json := HttpResponseBodyAsString( HttpGet('https://api.twitter.com/1.1/statuses/user_timeline.json', 443) );
	WriteLn(4, json);

	WriteLn(5, HttpResponseBodyAsString( HttpGet('http://www.swinburne.edu.au', 80) ) );
end;

begin
	Main();
end.
