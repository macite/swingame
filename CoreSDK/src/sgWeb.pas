//=============================================================================
// sgWeb.pas
//=============================================================================


/// SwinGame Web code allows you to create and access web resources. You can create a simple web server
/// or access web pages and applications.
///
/// @module Web
/// @static

unit sgWeb;

interface
  uses sgTypes;

  /// Converts the body of an HttpResponse to a string.
  ///
  /// @lib
  function HttpResponseBodyAsString(httpData: HttpResponse): String;

  // /// Returns the host name of a given ip address.
  // ///
  // /// @lib
  // function HostName(const address: String): String;

  // /// Returns the ip address of a given host.
  // ///
  // /// @lib
  // function HostIP(const name: String): String;

  /// Perform a get request for the resourse at the specified host, path and port.
  ///
  /// @lib
  function HttpGet(const url: String; port: Word): HttpResponse;

  /// Perform a post request to the specified host, with the supplied body.
  ///
  /// @lib
  function HttpPost(const url: String; port: Word; const body: String): HttpResponse;


  /// Free the resources used by the HttpResponse.
  ///
  /// @lib
  ///
  /// @class HttpResponse
  /// @dispose
  procedure FreeHttpResponse(var response: HttpResponse);


implementation
  uses sgDriverWeb, sgDriverSDL2Types, sgBackendTypes, sgShared;

  function HttpResponseBodyAsString(httpData: HttpResponse): String;
  var
    i: Integer;
    response: HttpResponsePtr;
  begin
    response := ToHttpResponsePtr(httpData);
    // WriteLn(response^.data.size);

    result := '';
    if not Assigned(response) then exit;

    for i := 0 to response^.data.size do
    begin
      // WriteLn(response^.data.data[i]);
      result += Char(response^.data.data[i]);
    end;
  end;

//----------------------------------------------------------------------------
// Http
//----------------------------------------------------------------------------

  // function HostName(const address: String): String;
  // var
  //   host: THostEntry;
  //   host6: THostEntry6;
  // begin
  //   result := '';
  //   if GetHostbyAddr(in_addr(StrToHostAddr(address)), host)
  //     or ResolveHostbyAddr(in_addr(StrToHostAddr(address)), host) then
  //     result := host.Name
  //   else if ResolveHostbyAddr6(StrToHostAddr6(address), host6) then
  //     result := host6.Name;
  // end;

  // function HostIP(const name: String): String;
  // var
  //   host: THostEntry;
  //   host6: THostEntry6;
  // begin
  //   result := '';
  //   if GetHostByName(name, host) or ResolveHostByName(name, host) then
  //     result := NetAddrToStr(host.Addr)
  //   else if ResolveHostByName6(name, host6) then
  //     result := NetAddrToStr6(host6.Addr);
  // end;

  function HttpGet(const url: String; port: Word): HttpResponse;
  var
    response : HttpResponsePtr;
  begin
    New(response);
    response^.id := HTTP_RESPONSE_PTR;
    response^.data := MakeRequest(HTTP_GET, url, port, '');
    result := HttpResponse(response);
  end;

  function HttpPost(const url: String; port: Word; const body: String): HttpResponse;
  var
    response : HttpResponsePtr;
  begin
    New(response);
    response^.id := HTTP_RESPONSE_PTR;
    response^.data := MakeRequest(HTTP_POST, url, port, body);
    result := HttpResponse(response);
  end;

  procedure FreeHttpResponse(var response: HttpResponse);
  var
    toFree: HttpResponsePtr;
  begin
    toFree := ToHttpResponsePtr(response);
    if Assigned(toFree) then Dispose(toFree);
    response := nil;
  end;


initialization
  begin
    InitialiseSwinGame();
  end;
end.
