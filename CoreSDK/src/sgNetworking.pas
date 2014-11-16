//=============================================================================
// sgNetworking.pas
//=============================================================================


/// The networking code of SwinGame is used for TCP and UDP connections to
/// and from multiple clients.
/// 
/// @module Networking
/// @static
unit sgNetworking;
//=============================================================================
interface
uses
  sgTypes;
//=============================================================================

//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns true if success or false
  /// if the binding failed. Uses TCP.
  ///
  /// @param aPort The port to bind the socket to.
  ///
  /// @lib
  /// @uname CreateTCPHost
  /// @sn createTCPHost:%s
  function CreateTCPHost              (aPort : LongInt) : Boolean;

  /// Opens a connection to a peer using the IP and port
  /// Creates a Socket for the purpose of two way messages. 
  /// Returns a new connection if successful or nil if failed.
  ///
  /// @param aIP The IP Address of the host
  /// @param aPort The port the host is listening to connections on
  ///
  /// @lib
  /// @uname CreateTCPConnectionToHost
  /// @sn createTCPConnection:%s port:%s
  function CreateTCPConnection    (aIP : String;  aPort : LongInt) : Connection;

  /// Accepts an incomming connection from another client.
  /// Returns the amount of new connections that have been
  /// accepted.
  ///
  /// @lib
  function AcceptTCPConnection  () : LongInt; 

  /// Attempts to recconnect a connection that was closed using the IP and port
  /// stored in the connection
  ///
  /// @param aConnection The connection to reconnect
  ///
  /// @lib
  /// @class Connection
  /// @method ReconnectConnection
  /// @self 1
  /// @sn reconnectConnection:%s
  procedure ReconnectConnection(var aConnection : Connection);
   
  /// Checks if a message has been received. If a message has been received,
  /// It will automatically add it to the message queue, with the message,
  /// source's IP and the port it received the message on. Returns true if
  /// a new message has been received.
  ///
  /// @lib
  function TCPMessageReceived         () : Boolean;    

  /// Broadcasts a message through all open connections.
  ///
  /// @param aMsg The message to be sent
  ///
  /// @lib
  /// @sn broadcastTCPMessage:%s
  procedure BroadcastTCPMessage        ( aMsg : String);

  /// Sends the message to the specified client, attached to the socket
  /// Retuns the connection if the message fails to
  /// send so that it may be closed. Returns nil if the message has been sent
  /// successfully.
  ///
  /// @param aMsg The message to be sent
  /// @param aConnection Send the message through this connection's socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendTCPMessage
  /// @self 2
  /// @sn sendTCPMessage:%s toConnection:%s
  function SendTCPMessage           ( aMsg : String; aConnection : Connection) : Connection;

//----------------------------------------------------------------------------
// HTTP
//----------------------------------------------------------------------------
  
  function HttpResponseBodyAsString(httpData: HTTPResponse): String;

  function HostName(const address: String): String;
  function HostIP(const name: String): String;

  /// Opens a HTTP Connection. This is the same as the TCP connection except
  /// that the HTTP flag is set. 
  /// Opens a connection to a peer using the IP and port
  /// Creates a Socket for the purpose of two way messages. 
  /// Returns a new connection if successful or nil if failed.
  ///
  /// @param aDestIP The IP Address of the host
  /// @param aDestPort The port the host is listening to connections on
  ///
  /// @lib
  /// @uname CreateHTTPConnection
  /// @sn createHTTPConnection:%s port:%s
  function CreateHTTPConnection(const aDestIP : String; const aDestPort : LongInt) : Connection;

  /// Sends the message to the specified server, attached to the socket
  /// Retuns the connection if the message fails to
  /// send so that it may be closed. Returns nil if the message has been sent
  /// successfully.
  ///
  /// @param aReq The HTTP Request message to be sent
  /// @param aConnection Send the message through this connection's socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendHTTPRequest
  /// @self 2
  /// @sn sendHTTPRequest:%s toConnection:%s
  // function SendHTTPRequest(const aReq : HTTPRequest; const aConnection : Connection) : Connection;

  /// Adds a header to the HTTP request with the name and value.
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param name The name of the header
  /// @param value The value of the header
  ///
  /// @lib
  /// @class Connection
  /// @method HTTPAddHeader
  /// @sn httpAddHeader:%s
  procedure HTTPAddHeader(var aHTTPRequest : HTTPRequest; const name, value : String);

  /// Removes a header of the HTTP request at the specified index.
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aIdx The index of the header
  ///
  /// @lib
  /// @class Connection
  /// @method httpRemoveHeaderAt
  /// @sn httpRemoveHeaderAt:%s
  procedure HTTPRemoveHeaderAt(var aHTTPRequest : HTTPRequest; const aIdx : LongInt);

  /// Returns a header of the HTTP Request at the specified index.
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aIdx The index of the header
  ///
  /// @lib
  /// @class Connection
  /// @method HTTPHeaderAt
  /// @sn httpHeaderAt:%s
  function HTTPHeaderAt(const aHTTPRequest : HTTPRequest; const aIdx : LongInt) : String;

  /// Returns a header of the HTTP Request at the specified index.
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aBody The body data
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetBody
  /// @sn httpSetBody:%s
  procedure HTTPSetBody(var aHTTPRequest : HTTPRequest; const aBody : String);

  /// Sets the method of the HTTP Request
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aMethod The type of request method
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetMethod
  /// @sn httpSetMethod:%s
  procedure HTTPSetMethod(var aHTTPRequest : HTTPRequest; const aMethod : HTTPMethod);

  /// Sets the version of the HTTP Request
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aVersion The version of the request
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetVersion
  /// @sn httpSetVersion:%s
  procedure HTTPSetVersion(var aHTTPRequest : HTTPRequest; const aVersion : String);

  /// Sets the URL of the HTTP Request
  ///
  /// @param aHTTPRequest The HTTP Request data
  /// @param aURL The URL for the HTTP Request
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetURL
  /// @sn httpSetURL:%s
  procedure HTTPSetURL(var aHTTPRequest : HTTPRequest; const aURL : String);

  /// Converts the HTTP Request to a string
  ///
  /// @param aHTTPRequest The HTTP Request data
  ///
  /// @lib
  /// @class Connection
  /// @method httpRequestToString
  /// @sn httpRequestToString:%s
  function HTTPRequestToString(const aHTTPRequest : HTTPRequest) : String;

  /// Encodes a string from username:password format to Base64
  ///
  /// @param aData The credentials
  ///
  /// @lib
  function EncodeBase64(const aData : String) : String;


  function HTTPGet(host: String; port: LongInt; path: String) : HTTPResponse;

//----------------------------------------------------------------------------
// Misc
//----------------------------------------------------------------------------

  /// Adds a connection to the list of new connections. This is called by the 
  /// Accept connection in TCP and Receive message in UDP (if the message has
  /// been sent by a new connection). This is used in conjunction with Fetch
  /// connection, that will pop the new connection out of the list.
  ///
  /// @param aConnection The new connection to add to the list
  ///
  /// @lib
  /// @class Connection
  /// @method EnqueueNewConnection
  /// @sn enqueueNewConnection:%s
  procedure EnqueueNewConnection(aConnection : Connection);

  /// Removes the top connection from the New connection queue and
  /// returns it.
  ///
  /// @lib
  function FetchConnection() : Connection;

  /// Returns the size of the New Connection List
  ///
  /// @lib
  function ConnectionQueueSize() : LongInt;
  
  /// Returns the count of Active Connections
  ///
  /// @lib
  function ConnectionCount() : LoNgInt;
  
  /// Retrieves the connection at the specified index
  ///
  /// @param aConnectionAt The index of the connection
  ///
  /// @lib
  function RetreiveConnection(aConnectionAt : LongInt) : Connection;

//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  /// Creates a socket that listens for connections based
  /// on the port given. Returns the index of the Socket in the
  /// socket array.
  ///
  /// @param aPort The port to bind the socket to.
  ///
  /// @lib
  /// @sn createUDPHost:%s
  function CreateUDPHost            ( aPort : LongInt) : LongInt;

  /// Creates the connection and sets the ip and port values. Creates a
  /// socket if there is no socket attached to the specified port. this
  /// socket can be used to send and receive messages. Returns the connection
  /// if this has been successful, or will return nil on failure.
  ///
  /// @param aDestIP The destination IP
  /// @param aDestPort The Destination Port
  /// @param aInPort The port to receive messages
  ///
  /// @lib
  /// @sn createUDPConnectionIP:%s port:%s inPort:%s
  function CreateUDPConnection        (aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 

  /// Checks all UDP listening sockets to see if a packet has been received.
  /// If a packet has been received, it will Enqueue the message into the message
  /// queue. This will set the message, sender's address and sender's port. it
  /// will return true if a message has been received or false if there has been
  /// no message.
  ///
  /// @lib
  function UDPMessageReceived         () : Boolean;

  /// Sends a UDP packet to the port and ip specified in the connection
  /// with the message.
  ///
  /// @param aMsg The message to be sent
  /// @param aConnection Send the Message through this connection's Socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendUDPMessage
  /// @self 2
  /// @sn sendUDPMessage:%s toConnection:%s
  function SendUDPMessage             ( aMsg : String; aConnection : Connection) : Boolean;
  
  /// Sends a UDP packet to All connections with the message.
  ///
  /// @param aMsg The message to be sent
  ///
  /// @lib
  /// @sn broadcastUDPMessage:%s
  procedure BroadcastUDPMessage( aMsg : String );

//----------------------------------------------------------------------------
// Messages and Connection Data Access
//----------------------------------------------------------------------------

  /// Gets the Decimal IP of the destination for the connection
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ConnectionIP
  /// @sn connectionIP:%s
  function  ConnectionIP(aConnection : Connection) : LongWord;

  /// Gets the Port of the destination for the connectiom
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ConnectionPort
  /// @sn connectionPort:%s
  function  ConnectionPort(aConnection : Connection) : LongInt;

  /// Dequeues the Top (Oldest) Message
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ReadMessage
  /// @sn readMessage:%s
  function ReadMessage            (aConnection : Connection) : String ;
  
  /// Dequeues the Last (Newest) Message
  ///
  /// @param aConnection The connection to extract data from
  ///
  /// @lib
  /// @class Connection
  /// @method ReadLastMessage
  /// @sn readLastMessage:%s
  function ReadLastMessage(aConnection : Connection) : String;

  /// Clears the Message Queue
  ///
  /// @lib
  /// @sn clearMessageQueue:%s
  ///
  /// @class Connection
  /// @method ClearMessageQueue
  procedure ClearMessageQueue          (aConnection : Connection) ;

  /// Gets the Size of the Message Queue
  ///
  /// @lib
  /// @sn messageCountOnConnection:%s
  ///
  /// @class Connection
  /// @method MessageCount
  function  MessageCount            (aConnection : Connection) : LongInt;

  /// Queues a message to the end of the Message Queue
  ///
  /// @param aMsg The message Sent
  /// @param aConnection The connection to enqueue the message into
  ///
  /// @lib
  /// @sn enqueueMessage:%s toConnection:%s
  ///
  /// @class Connection
  /// @method EnqueueMessage
  /// @self 2
  procedure EnqueueMessage            ( aMsg : String; aConnection : Connection);

  procedure EnqueueMessage( aMsg : String; const httpData: HTTPResponse; aConnection : Connection);

  procedure EnqueueMessage( const httpData: HTTPResponse; aConnection : Connection);


//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------

  /// Converts an Integer to a Hex value and returns it as a string.
  ///
  /// @param aDec The Integer
  ///
  /// @lib
  /// @uname DecToHex 
  /// @sn decToHex:%s
  function DecToHex                   (aDec : LongWord) : String;

  /// Converts a Hex String to a Decimal Value as a String.
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexToDecString 
  /// @sn hexToDecString:%s
  function HexToDecString             (aHex : String) : String;

  /// Converts a Hex String to an IPV4 Address (0.0.0.0)
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexStrToIPv4 
  /// @sn hexStrToIPv4:%s
  function HexStrToIPv4               (aHex : String) : String;

  /// Converts an IP to a decimal value
  ///
  /// @param aIP The IP
  ///
  /// @lib
  /// @sn iPv4ToDec:%s
  function IPv4ToDec(aIP : String) : LongWord; 

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------
  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Host Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  /// @uname CloseTCPHostSocket
  /// @sn closeTCPHostSocketPort:%s
  function CloseTCPHostSocket        ( aPort: LongInt) : Boolean;

  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to TCP Receiver Sockets
  ///
  /// @param aConnection  The Connection to close
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method Close
  function CloseConnection            (var aConnection : Connection) : Boolean;

  /// An internal function used to close the specified Socket. 
  /// Call ``CloseConnection`` instead.
  ///
  /// @param aConnection  The Connection to close
  ///
  /// @lib
  /// 
  /// @class Connection
  /// @dispose
  procedure FreeConnection(var aConnection : Connection);
  
  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to UDP Listener Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  function CloseUDPSocket      ( aPort : LongInt) : Boolean;

  /// Closes All TCP Host Sockets
  ///
  /// @lib
  procedure CloseAllTCPHostSockets     ();

  /// Closes All TCP Receiver Sockets
  ///
  /// @lib
  procedure CloseAllConnections ();

  /// Closes All UDP Listener Sockets
  ///
  /// @lib
  procedure CloseAllUDPSockets   ();

  /// Closes all sockets that have been created.
  ///
  /// @lib
  procedure CloseAllSockets ();

  /// Releases All resources used by the Networking code.
  ///
  /// @lib
  procedure ReleaseAllConnections();

//----------------------------------------------------------------------------
// Other
//----------------------------------------------------------------------------
  /// Returns the caller's IP.
  ///
  /// @lib
  function MyIP                  () : String;
          
//=============================================================================
implementation
  uses SysUtils, sgUtils, sgDriverNetworking, sgNamedIndexCollection, 
    {$ifdef WINDOWS}
      Winsock2,
    {$endif}
    {$ifdef UNIX}
      BaseUnix, NetDB,
    {$endif}
    Sockets, sgShared, StrUtils, sgDriverSDL2Types;
//=============================================================================

type
  MessageData = packed record
    msg       : String;
    IP        : String;
    port      : LongInt;
  end;
  MessageDataArray = Array of MessageData;

  NewConnectionPtr = ^NewConnection;
  NewConnection = record
    con : Connection;
    nextCon : NewConnectionPtr;
  end;

var  
  _NewConnectionQueue     : NewConnectionPtr = nil;
  _NewConnectionCount     : LongInt = 0;
  
//----------------------------------------------------------------------------
// Hexadecimal and Decimal Conversion
//----------------------------------------------------------------------------
    
  function DecToHex(aDec : LongWord) : String;
  var
    LRemainder : LongWord;
    lHexAlpha : String = '0123456789ABCDEF';
  begin
    lRemainder := (aDec mod 16);
    if aDec - lRemainder = 0 then
      result := lHexAlpha[lRemainder + 1]
    else 
      result := DecToHex( (aDec - lRemainder) div 16 ) + lHexAlpha[lRemainder + 1]
  end;
      
  function HexToDecString(aHex : String) : String;
  var
    i    : LongInt;
    lVal  : LongInt = 0;
    lExpo : Double;
  begin
    for i := 1 to Length(aHex) do
    begin      
      lExpo := Exp((Length(aHex) - i)*Ln(16));
      case aHex[i] of
        '0' : lVal += Round(0  * lExpo);
        '1' : lVal += Round(1  * lExpo);
        '2' : lVal += Round(2  * lExpo);
        '3' : lVal += Round(3  * lExpo);
        '4' : lVal += Round(4  * lExpo);
        '5' : lVal += Round(5  * lExpo);
        '6' : lVal += Round(6  * lExpo);
        '7' : lVal += Round(7  * lExpo);
        '8' : lVal += Round(8  * lExpo); 
        '9' : lVal += Round(9  * lExpo);
        'A' : lVal += Round(10 * lExpo);
        'B' : lVal += Round(11 * lExpo);
        'C' : lVal += Round(12 * lExpo);
        'D' : lVal += Round(13 * lExpo);
        'E' : lVal += Round(14 * lExpo);
        'F' : lVal += Round(15 * lExpo);
      end;
    end;   
    result := IntToStr(lVal); 
  end;
  
  function HexStrToIPv4(aHex : String) : String;
  begin
    result :=       HexToDecString(aHex[1] + aHex[2]);
    result += '.' + HexToDecString(aHex[3] + aHex[4]);
    result += '.' + HexToDecString(aHex[5] + aHex[6]);
    result += '.' + HexToDecString(aHex[7] + aHex[8]);
  end;

  function IPv4ToDec(aIP : String) : LongWord;
  var
    w, x, y, z : LongInt;
  begin
    w := StrToInt(ExtractDelimited(1, aIP, ['.']));
    x := StrToInt(ExtractDelimited(2, aIP, ['.']));
    y := StrToInt(ExtractDelimited(3, aIP, ['.']));
    z := StrToInt(ExtractDelimited(4, aIP, ['.']));
    result := 16777216 * w + 65536 * x + 256 * y + z;
    // WriteLn('Result: ', result);
  end;

  function Encode(const c : Byte) : Char;
  begin
    if (c < 26) then
      result := Char(65 + c)        //65 = A
    else if (c < 52) then
      result := Char(97 + (c - 26)) // 97 = a
    else if (c < 62) then
      result := Char(48 + (c - 52)) //48 = 0
    else if (c = 62) then
      result := Char(43)           //43 = +
    else result := Char(47);       //47 = /
  end;

  function EncodeBase64(const aData : String) : String;
  var
    i : Integer = 1;
    lC1, lC2, lC3, lC4, lC5, lC6, lC7 : Char;
    lLen : Integer;
  begin
    result := '';

    if Length(aData) = 0 then exit;

    lLen := Length(aData) + 1;

    while i < lLen do
    begin
      lC1 := aData[i];

      if i + 1 < lLen then
        lC2 := aData[i + 1]
      else
        lC2 := Char(0);

      if i + 2 < lLen then
        lC3 := aData[i + 2]
      else
        lC3 := Char(0);

      lC4 := Char(Byte(lC1) >> 2);
      lC5 := Char(((Byte(lC1) and Byte($03)) << 4) or (Byte(lC2) >> 4)); 
      lC6 := Char(((Byte(lC2) and Byte($0F)) << 2) or (Byte(lC3) >> 6)); 
      lC7 := Char(Byte(lC3) and Byte($3F)); 

      result += Encode(Byte(lC4));
      result += Encode(Byte(lC5));

      if i + 1 < lLen then
        result += Encode(Byte(lC6))
      else
        result += '=';

      if i + 2 < lLen then
        result += Encode(Byte(lC7))
      else
        result += '=';

      if ((i mod (74 div 4*3)) = 0) then
        result += #13#10;
      i += 3;
    end;
  end;
  
//----------------------------------------------------------------------------
// TCP
//----------------------------------------------------------------------------
    
  function CreateTCPHost( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CreateTCPHost(aPort);
  end;
  
  function CreateTCPConnection( aIP : String;  aPort : LongInt) : Connection;
  begin
    result := NetworkingDriver.CreateTCPConnection(aIP, aPort, TCP);
  end;   

  procedure ReconnectConnection(var aConnection : Connection);
  var
    lIPString : String;
    lPort : LongInt;
  begin
    lIPString := aConnection^.stringIP;
    lPort := aConnection^.port;

    CloseConnection(aConnection);
    aConnection := NetworkingDriver.CreateTCPConnection(lIPString, lPort, HTTP);
  end;
  
  function AcceptTCPConnection() : LongInt;
  begin
    result := NetworkingDriver.AcceptTCPConnection();
  end;
  
//----------------------------------------------------------------------------
// Connection
//----------------------------------------------------------------------------

  procedure EnqueueNewConnection(aConnection : Connection);
  var
    lNewConLink : NewConnectionPtr;
    lLastLink : NewConnectionPtr = nil;
  begin
    New(lNewConLink);  
    lNewConLink^.con        := aConnection;
    lNewConLink^.nextCon    := nil;

    if _NewConnectionQueue = nil then
      _NewConnectionQueue   := lNewConLink
    else begin
      lLastLink := _NewConnectionQueue;

      while lLastLink^.nextCon <> nil do
        lLastLink := lLastLink^.nextCon;

      lLastLink^.nextCon := lNewConLink;
    end;  
    _NewConnectionCount += 1; 
  end;

  function FetchConnection() : Connection;
  var
    lTmp : NewConnectionPtr;
  begin  
    result := nil;
    if _NewConnectionQueue = nil then exit;

    result := _NewConnectionQueue^.con;

    lTmp := _NewConnectionQueue^.nextCon;
    Dispose(_NewConnectionQueue);
    _NewConnectionQueue := lTmp;
    _NewConnectionCount -= 1; 
  end;

  function ConnectionQueueSize() : LongInt;
  begin
    result := _NewConnectionCount;
  end;
  
  function ConnectionCount() : LongInt;
  begin
    result := NetworkingDriver.ConnectionCount();
  end;

  function RetreiveConnection(aConnectionAt : LongInt) : Connection;
  begin
    result := NetworkingDriver.RetreiveConnection(aConnectionAt);
  end;
  
//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------
  
  procedure EnqueueMessage( aMsg : String; const httpData: HTTPResponse; aConnection : Connection);
  var
    msgData   : MessagePtr;
  begin
    if not Assigned(aConnection) then exit;

    New(msgData); 
    msgData^.data := aMsg;
    msgData^.httpData := httpData;
    msgData^.next := nil;
    msgData^.prev := aConnection^.lastMsg;

    if aConnection^.firstMsg = nil then
    begin
      aConnection^.firstMsg := msgData;
    end else begin
      aConnection^.lastMsg^.next := msgData;
    end;  

    aConnection^.lastMsg  := msgData;
    aConnection^.msgCount += 1;
  end;

  procedure EnqueueMessage( aMsg : String; aConnection : Connection);
  var
    httpData: HTTPResponse;
  begin
    httpData.protocol := '';
    httpData.status  := 0;
    httpData.statusText := '';
    SetLength(httpData.headers, 0);
    SetLength(httpData.body, 0);

    EnqueueMessage( aMsg, httpData, aConnection );
  end;

  function HttpResponseBodyAsString(httpData: HTTPResponse): String;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to High(httpData.body) do
    begin
      result += Char(httpData.body[i]);
    end;
  end;

  procedure EnqueueMessage(const httpData: HTTPResponse; aConnection : Connection);
  begin
    EnqueueMessage( HttpResponseBodyAsString(httpData), httpData, aConnection);
  end;


   
  function ReadMessage(aConnection : Connection) : String;
  var
    lTmp : MessagePtr;
  begin      
    result := '';
    if not Assigned(aConnection) or (aConnection^.msgCount = 0) then exit;
    if not Assigned(aConnection^.firstMsg) then begin aConnection^.lastMsg := nil; exit; end; 

    // Get the data from the first message
    result := aConnection^.firstMsg^.data;

    // Get new first message (may be nil)
    lTmp := aConnection^.firstMsg^.next;

    Dispose(aConnection^.firstMsg);
    aConnection^.firstMsg := lTmp;
    aConnection^.msgCount -= 1;

    // If there is a first node then, remove its prev link
    if Assigned(lTmp) then
    begin
      lTmp^.prev := nil;
    end;

    if aConnection^.msgCount = 0 then
    begin
      aConnection^.lastMsg := nil;
      aConnection^.firstMsg := nil;
    end;
  end;
  
  function ReadLastMessage(aConnection : Connection) : String;
  var
    lTmp : MessagePtr;
  begin
    result := '';      
    if not Assigned(aConnection) or (aConnection^.msgCount = 0) then exit;
    if not Assigned(aConnection^.lastMsg) then begin aConnection^.firstMsg := nil; exit; end; 

    // Get the message text from the last node
    result := aConnection^.lastMsg^.data;

    // Remember so we can dispose
    lTmp := aConnection^.lastMsg;

    // Change last message
    aConnection^.lastMsg := aConnection^.lastMsg^.prev;

    // Delete old node
    Dispose(lTmp);
    aConnection^.msgCount -= 1;

    // If there is a node, then remove its next
    if Assigned(aConnection^.lastMsg) then
    begin
      // New last message is not followed by anything
      aConnection^.lastMsg^.next := nil;
    end;

    if aConnection^.msgCount = 0 then
    begin
      aConnection^.lastMsg := nil;
      aConnection^.firstMsg := nil;
      exit;
    end;
  end;
  
  procedure ClearMessageQueue(aConnection : Connection);
  var
    i : LongInt;
  begin
    if not Assigned(aConnection) then exit;
    
    for i := 0 to aConnection^.msgCount do
    begin
      ReadMessage(aConnection);
    end;
  end;  

  function ConnectionIP(aConnection : Connection) : LongWord;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.ip;
  end;

  function ConnectionPort(aConnection : Connection) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.port;
  end;

  function MessageCount(aConnection : Connection) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.msgCount;
  end;

  function TCPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.TCPMessageReceived();
  end;
  
  procedure BroadcastTCPMessage(aMsg : String);
  begin
    NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;
  
  function SendTCPMessage( aMsg : String; aConnection : Connection) : Connection;
  begin
    result := NetworkingDriver.SendTCPMessage(aMsg, aConnection);
  end;

//----------------------------------------------------------------------------
// HTTP
//----------------------------------------------------------------------------

  function HostName(const address: String): String;
  var
    host: THostEntry;
    host6: THostEntry6;
  begin
    result := '';
    if GetHostbyAddr(in_addr(StrToHostAddr(address)), host) 
      or ResolveHostbyAddr(in_addr(StrToHostAddr(address)), host) then
      result := host.Name
    else if ResolveHostbyAddr6(StrToHostAddr6(address), host6) then
      result := host6.Name;
  end;

  function HostIP(const name: String): String;
  var
    host: THostEntry;
    host6: THostEntry6;
  begin
    result := '';
    if GetHostByName(name, host) or ResolveHostByName(name, host) then
      result := NetAddrToStr(host.Addr)
    else if ResolveHostByName6(name, host6) then
      result := NetAddrToStr6(host6.Addr);
  end;

  function CreateHTTPConnection(const aDestIP : String; const aDestPort : LongInt) : Connection;
  begin
    result := NetworkingDriver.CreateTCPConnection(aDestIP, aDestPort, HTTP);
  end;
  
  procedure SendHTTPRequest(const aReq : HttpRequest; const aConnection : sg_network_connection);
  var
    lLen, i : LongInt;
    buffer : Array of Char;
    lMsg : String = '';
  begin
    // result := nil;
    // if (aConnection = nil) or (aConnection^.socket = nil) or (aConnection^.conType <> HTTP) then 
    // begin 
    //   RaiseWarning('SDL 1.2 SendTCPMessageProcedure Illegal Connection Arguement (nil or not HTTP)'); 
    //   exit; 
    // end;

    lMsg := HTTPRequestToString(aReq) + #13#10#13#10;
    SetLength(buffer, Length(lMsg));

    for i := 0 to Length(lMsg) - 1 do
      buffer[i] := lMsg[i + 1];
    
    lLen := Length(lMsg);
    
    // WriteLn(lMsg);

    if _sg_functions^.network.send_bytes(@aConnection, @buffer[0], lLen) < lLen then
    // if (SDLNet_TCP_Send(aConnection^.socket, @buffer[0], lLen) < lLen) then
    begin
      // RaiseWarning('Error sending message: SDLNet_TCP_Send: ' + SDLNet_GetError() + ' HTTP Connection may have been refused.');
    end;
  end;

  procedure HTTPAddHeader(var aHTTPRequest : HTTPRequest; const name, value : String);
  begin
    SetLength(aHTTPRequest.headername, Length(aHTTPRequest.headername) + 1);
    SetLength(aHTTPRequest.headervalue, Length(aHTTPRequest.headervalue) + 1);
    aHTTPRequest.headername[High(aHTTPRequest.headername)] := name;
    aHTTPRequest.headervalue[High(aHTTPRequest.headervalue)] := value;
  end;

  procedure HTTPRemoveHeaderAt(var aHTTPRequest : HTTPRequest; const aIdx : LongInt);
  var
    i : Integer;
  begin
    for i := aIdx to High(aHTTPRequest.headername) do 
    begin
      if i = High(aHTTPRequest.headername) then continue;

      aHTTPRequest.headername[i] := aHTTPRequest.headername[i + 1];
      aHTTPRequest.headervalue[i] := aHTTPRequest.headervalue[i + 1];
    end;

    SetLength(aHTTPRequest.headername, Length(aHTTPRequest.headername) - 1);
    SetLength(aHTTPRequest.headervalue, Length(aHTTPRequest.headervalue) - 1);
  end;

  function HTTPHeaderAt(const aHTTPRequest : HTTPRequest; const aIdx : LongInt) : String;
  begin
    result := '';
    if (aIdx < 0) or (aIdx > High(aHTTPRequest.headername)) then exit;

    result := aHTTPRequest.headername[aIdx] + ': '+ aHTTPRequest.headervalue[aIdx];
  end;

  procedure HTTPSetBody(var aHTTPRequest : HTTPRequest; const aBody : String);
  begin
    aHTTPRequest.body := aBody;
  end;

  procedure HTTPSetMethod(var aHTTPRequest : HTTPRequest; const aMethod : HTTPMethod);
  begin
    aHTTPRequest.requestType := aMethod;
  end;

  procedure HTTPSetVersion(var aHTTPRequest : HTTPRequest; const aVersion : String);
  begin
    aHTTPRequest.version := aVersion;
  end;

  procedure HTTPSetURL(var aHTTPRequest : HTTPRequest; const aURL : String);
  begin
    aHTTPRequest.url := aURL;
  end;

  function HTTPRequestToString(const aHTTPRequest : HTTPRequest) : String;
  var
    i, len : Integer;
  begin
    result := '';
    case aHTTPRequest.requestType of
      HTTP_GET: result += 'GET ';
      HTTP_POST: result += 'POST ';
      HTTP_PUT: result += 'PUT ';
      HTTP_DELETE: result += 'DELETE ';
    end;
    result += aHTTPRequest.url;
    result += ' HTTP/' + aHTTPRequest.version;
    result += #13#10;
    for i := Low(aHTTPRequest.headername) to High(aHTTPRequest.headername) do
      result += aHTTPRequest.headername[i] + ': ' + aHTTPRequest.headervalue[i] + #13#10;
    len := Length(aHTTPRequest.body);

    if len <> 0 then
      result += 'Content-Length: ' + IntToStr(len) + #13#10#13#10;
    result += aHTTPRequest.body;
  end;

  function ExtractLine(const aConnection: sg_network_connection): String;
  var
    aReceivedCount: Integer;
    ch: Char;
  begin
    result := '';

    while true do
    begin
      aReceivedCount := _sg_functions^.network.read_bytes(@aConnection, @ch, 1) ; //SDLNet_TCP_Recv(aConnection^.socket, @ch, 1);

      if aReceivedCount = 0 then 
        exit;

      if ch = #13 then // skip cr
        continue;
      if ch = #10 then // end at lf
        exit;
      
      result += ch;
    end;
  end;

  // procedure ReadBytes(const aConnection: sg_network_connection; buffer: BytePtr; var size: Integer);
  // // var
  // //   i: Integer;
  // begin
  //   size := SDLNet_TCP_Recv(aConnection^.socket, buffer, size);
  //   // WriteLn('read ', size);
  //   // for i := 0 to size - 1 do
  //   // begin
  //   //   Write(Char((buffer + i)^));
  //   // end;
  // end;

  function GetHeader(const response: HttpResponse; const name: String; var header: HttpHeader): Boolean;
  var
    i: Integer;
  begin
    result := false;

    for i := 0 to High(response.headers) do
    begin
      if CompareText(response.headers[i].name, name) = 0 then
      begin
        header := response.headers[i];
        result := true;
        exit;
      end;
    end;
  end;

  procedure ReadHeaders(const aConnection: sg_network_connection; var response: HttpResponse);
  var
    header: HttpHeader;
    line: String;
    pos: Integer;
  begin
    header.value := '';
    header.name := '';

    line := ExtractLine(aConnection);

    while Length(line) > 0 do
    begin
      // WriteLn(line);
      if (line[1] = ' ') or (line[1] = #9) then
      begin
         // add to last header
         header.value += Trim(line);
      end
      else
      begin
        pos := 1;
        // new header
        header.name := Trim(ExtractSubstr(line, pos, [ ':' ]));
        header.value := Trim(ExtractSubstr(line, pos, [ #13, #10 ]));

        SetLength(response.headers, Length(response.headers) + 1);
      end;

      // copy in header to last location -- override in case of extending value
      response.headers[High(response.headers)] := header;

      // WriteLn('GOT Header: ', response.headers[High(response.headers)].name);
      // WriteLn('    Value : ', response.headers[High(response.headers)].value);

      line := ExtractLine(aConnection); // read next line
    end;
  end;

  function ReadHttpResponse(const aConnection : sg_network_connection): HttpResponse;
  var
    header: HttpHeader;
    line : String;
    i, pos, size, readSize, code: Integer;
  begin
    try
      result.protocol := 'HTTP/1.1';
      result.status := 500;
      result.statusText := 'Internal Server Error';
      SetLength(result.body, 0);
      SetLength(result.headers, 0);

      // first line is status and protocol
      line := ExtractLine(aConnection);
      // WriteLn('line: ', line);
      if length(line) = 0 then exit; //end but add message
      pos := 1;

      result.protocol := ExtractSubstr(line, pos, [ ' ' ]);
      TryStrToInt(ExtractSubstr(line, pos, [ ' ' ]), result.status);
      result.statusText := ExtractSubstr(line, pos, [ #13, #10 ]);
      // WriteLn('GOT ', result.protocol, ' ', result.status, ' ', result.statusText);
      
      ReadHeaders(aConnection, result);

      // Read body
      // Look for content size header
      if GetHeader(result, 'Content-Length', header) and TryStrToInt(header.value, size) then
      begin
        // read a fixed size body
        SetLength(result.body, size);
        _sg_functions^.network.read_bytes(@aConnection, @result.body[0], size);
      end
      else if GetHeader(result, 'Transfer-Encoding', header) and ( CompareText(header.value, 'chunked') = 0 ) then
      begin
        // read body in chunks
        line := ExtractLine(aConnection);
        // WriteLn(line);
        size := 0;
        pos := 0;
        Val('x' + Trim(ExtractSubstr(line, pos, [ ';' ])), size, code);

        while size > 0 do
        begin
          i := Length(result.body);
          // WriteLn(line, ' = ', size, ' ', i + size) ;
          SetLength(result.body, i + size); // make more space
          readSize := _sg_functions^.network.read_bytes(@aConnection, @result.body[i], size); // read in next chunk
          while readSize < size do
          begin
            size -= readSize;
            i += readSize;
            readSize := _sg_functions^.network.read_bytes(@aConnection, @result.body[i], size); // read in next chunk
          end;

          // WriteLn('size - ', size, ' = ', HttpResponseBodyAsString(result));
          // WriteLn(#10);

          size := 0;
          pos := 0;
          line := ExtractLine(aConnection); // read the #13#10 after body before next chunk
          // WriteLn(' got --> ', line);
          if Length(line) = 0 then
            line := ExtractLine(aConnection); // read size of next chunk
          // WriteLn(' got --> ', line);
          if Length(line) > 0 then
            Val('x' + Trim(ExtractSubstr(line, pos, [ ';' ])), size, code);
        end;

        if code <> 0 then
        begin
          WriteLn('Error reading size of chunk from: ', line);
          exit;
        end;

        // read the footers...
        ReadHeaders(aConnection, result);
        // WriteLn('here - ', HttpResponseBodyAsString(result));
      end;
    // except
    finally
      // EnqueueMessage(result, aConnection);  
    end;
  end;

  function HTTPGet(host: String; port: LongInt; path: String) : HTTPResponse;
  var
    con : sg_network_connection;
    request : HTTPRequest;
  begin
    // ip := HostIP(host);
    // ip := host;
    HTTPAddHeader(request, 'Host', host + ':' + IntToStr(port));
    HTTPAddHeader(request, 'Connection', 'close');

    // Create HTTP message
    HTTPSetMethod(request, HTTP_GET);
    HTTPSetURL(request, path);
    HTTPSetVersion(request, '1.1');

    HTTPSetBody(request, '');
    
    con := _sg_functions^.network.open_tcp_connection(PChar(host), port);

    // CreateHTTPConnection(ip, port);
    SendHTTPRequest(request, con);
    result := ReadHttpResponse(con);
    // WriteLn('here - ', HttpResponseBodyAsString(result));
    _sg_functions^.network.close_connection(@con);
  end;



//----------------------------------------------------------------------------
// UDP
//----------------------------------------------------------------------------

  function CreateUDPHost( aPort : LongInt) : LongInt;
  begin
    result := NetworkingDriver.CreateUDPHost(aPort);
  end;
  
  function CreateUDPConnection(aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 
  begin
    result := NetworkingDriver.CreateUDPConnection(aDestIP, aDestPort, aInPort);
  end;

  function UDPMessageReceived() : Boolean;
  begin
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function SendUDPMessage( aMsg : String; aConnection : Connection) : Boolean;
  begin
    result := NetworkingDriver.SendUDPMessage(aMsg, aConnection);
  end;
  
  procedure BroadcastUDPMessage( aMsg : String );
  begin
    NetworkingDriver.BroadcastUDPMessage(aMsg);
  end;

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------

  function CloseTCPHostSocket( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseTCPHostSocket(aPort);    
  end;

  function CloseConnection(var aConnection : Connection) : Boolean;
  begin
    result := NetworkingDriver.CloseConnection(aConnection, False);    
  end;

  function CloseUDPSocket( aPort : LongInt) : Boolean;
  begin
    result := NetworkingDriver.CloseUDPSocket(aPort);    
  end;

  procedure CloseAllTCPHostSockets();
  begin     
    NetworkingDriver.CloseAllTCPHostSocket();    
  end;

  procedure CloseAllConnections();
  begin        
    NetworkingDriver.CloseAllConnections(False);    
  end;

  procedure CloseAllUDPSockets();
  begin        
    NetworkingDriver.CloseAllUDPSocket();    
  end;

  procedure CloseAllSockets();
  begin
    CloseAllTCPHostSockets();
    CloseAllConnections();
    CloseAllUDPSockets();
  end;

  procedure FreeConnection(var aConnection : Connection);
  begin
    CallFreeNotifier(aConnection);
    Dispose(aConnection);
    aConnection := nil;
  end;

  procedure ReleaseAllConnections();
  var
    i : LongInt;
  begin
    NetworkingDriver.FreeAllNetworkingResources();

    for i := 0 to _NewConnectionCount - 1 do
      FetchConnection();
    _NewConnectionCount := 0;
  end;

//----------------------------------------------------------------------------
// Other
//----------------------------------------------------------------------------

  function MyIP() : String;
  begin
    result := NetworkingDriver.MyIP();
  end;

//=============================================================================

end.