unit sgDriverWeb;

interface
	uses sgDriverSDL2Types, sgBackendTypes, sgTypes;

	function MakeRequest (request_type: sg_http_method; url: String; port: Word; body: String): sg_http_response;

    procedure FreeResponse (var response: sg_http_response);

implementation
	uses sgInputBackend, sgShared, sgWindowManager, sgGeometry;

	function MakeRequest (request_type: sg_http_method; url: String; port: Word; body: String): sg_http_response;
	var
		request: sg_http_request;
	begin
		request.request_type := request_type;
		request.url := PChar(url);
		request.port := port;
		request.body := PChar(body);

		result := _sg_functions^.web.http_request(request);
	end;

	procedure FreeResponse (var response: sg_http_response);
	begin
		_sg_functions^.web.free_response(@response);
	end;

end.
