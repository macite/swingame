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

  /// Creates a server socket that listens for TCP connections 
  /// on the port given. Returns the server if this succeeds, otherwise
  /// it returns nil/null.
  ///
  /// @param port The number of the port to listen to for new connections
  ///
  /// @lib
  /// @sn createTCPServerNamed:%s onPort:%s
  function CreateTCPServer(const name: String; port : LongInt) : ServerSocket;

  /// Opens a connection to a server using the IP and port.
  /// Creates a Connection for the purpose of two way messages. 
  /// Returns a new connection if successful or nil/null if it fails.
  ///
  /// @param aIP The IP Address or DNS name of the host
  /// @param aPort The port number the server is using to listen for connections
  ///
  /// @lib
  /// @sn openTCPConnection:%s port:%s
  function OpenTCPConnection(const host: String; port: LongInt) : Connection;

  /// Check all servers for incomming connections from clients.
  /// Returns the number of new connections that have been
  /// accepted.
  ///
  /// @lib
  function AcceptAllNewConnections () : LongInt;

  /// Check the server for incomming connections from clients.
  /// Returns the number of new connections that have been
  /// accepted.
  ///
  /// @lib
  function AcceptNewConnection (server: ServerSocket) : LongInt;

  // /// Check the server (based on its name) for incomming connections from clients.
  // /// Returns the number of new connections that have been
  // /// accepted.
  // ///
  // /// @lib AcceptNewConnectionByName
  // function AcceptNewConnections (const server: String) : LongInt;

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
   
  /// Checks if any messages have been received for any open connections. 
  /// Messages received are added to the connection they were received from.
  ///
  /// @lib
  function MessagesReceived () : Boolean;

  /// Broadcasts a message through all open connections.
  ///
  /// @param aMsg The message to be sent
  ///
  /// @lib
  /// @sn broadcastTCPMessage:%s
  procedure BroadcastTCPMessage        ( aMsg : String);

  /// Sends the message over the provided network connection.
  /// Returns true if this succeeds, or false if it fails.
  ///
  /// @param aMsg The message to be sent
  /// @param aConnection Send the message through this connection
  ///
  /// @lib
  /// @sn sendMessage:%s toConnection:%s
  ///
  /// @class Connection
  /// @method SendMessage
  /// @self 2
  function SendMessageTo(const aMsg : String; aConnection : Connection) : Boolean;


  /// Check for any messages that have come in from Clients. 
  /// This is called automatically by ProcessEvents, but can be called
  /// manually.
  ///
  /// @lib
  procedure CheckNetworkActivity();

//----------------------------------------------------------------------------
// Http
//----------------------------------------------------------------------------
  
  function HttpResponseBodyAsString(httpData: HttpResponse): String;

  function HostName(const address: String): String;
  function HostIP(const name: String): String;

  /// Opens a Http Connection. This is the same as the TCP connection except
  /// that the Http flag is set. 
  /// Opens a connection to a peer using the IP and port
  /// Creates a Socket for the purpose of two way messages. 
  /// Returns a new connection if successful or nil if failed.
  ///
  /// @param aDestIP The IP Address of the host
  /// @param aDestPort The port the host is listening to connections on
  ///
  /// @lib
  /// @uname CreateHttpConnection
  /// @sn createHttpConnection:%s port:%s
  function CreateHttpConnection(const aDestIP : String; const aDestPort : LongInt) : Connection;

  /// Sends the message to the specified server, attached to the socket
  /// Retuns the connection if the message fails to
  /// send so that it may be closed. Returns nil if the message has been sent
  /// successfully.
  ///
  /// @param aReq The Http Request message to be sent
  /// @param aConnection Send the message through this connection's socket.
  ///
  /// @lib
  /// @class Connection
  /// @method SendHttpRequest
  /// @self 2
  /// @sn sendHttpRequest:%s toConnection:%s
  // function SendHttpRequest(const aReq : HttpRequest; const aConnection : Connection) : Connection;

  /// Adds a header to the Http request with the name and value.
  ///
  /// @param aHttpRequest The Http Request data
  /// @param name The name of the header
  /// @param value The value of the header
  ///
  /// @lib
  /// @class Connection
  /// @method HttpAddHeader
  /// @sn httpAddHeader:%s
  procedure HttpAddHeader(var aHttpRequest : HttpRequest; const name, value : String);

  /// Removes a header of the Http request at the specified index.
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aIdx The index of the header
  ///
  /// @lib
  /// @class Connection
  /// @method httpRemoveHeaderAt
  /// @sn httpRemoveHeaderAt:%s
  procedure HttpRemoveHeaderAt(var aHttpRequest : HttpRequest; const aIdx : LongInt);

  /// Returns a header of the Http Request at the specified index.
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aIdx The index of the header
  ///
  /// @lib
  /// @class Connection
  /// @method HttpHeaderAt
  /// @sn httpHeaderAt:%s
  function HttpHeaderAt(const aHttpRequest : HttpRequest; const aIdx : LongInt) : String;

  /// Returns a header of the Http Request at the specified index.
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aBody The body data
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetBody
  /// @sn httpSetBody:%s
  procedure HttpSetBody(var aHttpRequest : HttpRequest; const aBody : String);

  /// Sets the method of the Http Request
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aMethod The type of request method
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetMethod
  /// @sn httpSetMethod:%s
  procedure HttpSetMethod(var aHttpRequest : HttpRequest; const aMethod : HttpMethod);

  /// Sets the version of the Http Request
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aVersion The version of the request
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetVersion
  /// @sn httpSetVersion:%s
  procedure HttpSetVersion(var aHttpRequest : HttpRequest; const aVersion : String);

  /// Sets the URL of the Http Request
  ///
  /// @param aHttpRequest The Http Request data
  /// @param aURL The URL for the Http Request
  ///
  /// @lib
  /// @class Connection
  /// @method httpSetURL
  /// @sn httpSetURL:%s
  procedure HttpSetURL(var aHttpRequest : HttpRequest; const aURL : String);

  /// Converts the Http Request to a string
  ///
  /// @param aHttpRequest The Http Request data
  ///
  /// @lib
  /// @class Connection
  /// @method httpRequestToString
  /// @sn httpRequestToString:%s
  function HttpRequestToString(const aHttpRequest : HttpRequest) : String;

  /// Encodes a string from username:password format to Base64
  ///
  /// @param aData The credentials
  ///
  /// @lib
  function EncodeBase64(const aData : String) : String;


  function HttpGet(host: String; port: LongInt; path: String) : HttpResponse;

//----------------------------------------------------------------------------
// Misc
//----------------------------------------------------------------------------

  /// Returns the number of connections to a Server socket.
  ///
  /// @lib
  function ConnectionCount(server: ServerSocket) : LongInt;
  
  /// Retrieves the connection at the specified index
  ///
  /// @param idx The index of the connection
  ///
  /// @lib
  function RetreiveConnection(server: ServerSocket; idx: LongInt) : Connection;

  /// Returns the last connection made to a server socket.
  ///
  /// @lib
  function LastConnection(server: ServerSocket) : Connection;

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
  function CreateUDPHost ( aPort : LongInt) : LongInt;

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
  function CreateUDPConnection(aDestIP : String; aDestPort, aInPort : LongInt) : Connection; 

  /// Checks all UDP listening sockets to see if a packet has been received.
  /// If a packet has been received, it will Enqueue the message into the message
  /// queue. This will set the message, sender's address and sender's port. it
  /// will return true if a message has been received or false if there has been
  /// no message.
  ///
  /// @lib
  function UDPMessageReceived() : Boolean;

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
  function SendUDPMessage( aMsg : String; aConnection : Connection) : Boolean;
  
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
  procedure EnqueueMessage( const aMsg : String; aConnection : Connection);


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
  ServerArray = array of ServerSocket;
  ConnectionArray = array of Connection;
  PacketData = array [0..511] of Char;
  BytePtr = ^Byte;

var
  _Servers                : ServerArray;
  _Connections            : ConnectionArray;
  
  // _UDPListenSockets       : UDPSocketArray;
  // _UDPSocketIDs           : NamedIndexCollection;
  // _UDPConnectionIDs       : NamedIndexCollection;

  // _UDPSendPacket          : PUDPPacket = nil;
  // _UDPReceivePacket       : PUDPPacket = nil;


//----------------------------------------------------------------------------
// Internal Functions
//----------------------------------------------------------------------------

  function CreateConnection() : Connection;
  begin
    New(result);
    result^.socket      := nil;
    result^.ip          := 0;
    result^.stringIP    := '';
    result^.port        := 0;
    result^.firstmsg    := nil;
    result^.lastMsg     := nil;
    result^.msgCount    := 0;
    result^.protocol     := TCP;
    result^.partMsgData := '';
    result^.msgLen      := -1;
  end;

  function GetConnectionWithID(const aIP : LongWord; const aPort : LongInt; aprotocol : ConnectionType) : Connection;
  var
    i : LongInt;
  begin
    result := nil;
    for i := Low(_Connections) to High(_Connections) do
    begin
      if (_Connections[i]^.ip = aIP) and (_Connections[i]^.port = aPort) and (_Connections[i]^.protocol = aprotocol) then
      begin
        result := _Connections[i];
      end;
    end;
  end;

  procedure ExtractData(const buffer: PacketData; aReceivedCount: LongInt; const aConnection : Connection);
  var
    msgLen    : Byte = 0;
    bufIdx    : LongInt = 0;               // Index of current data in the buffer
    msg       : String;
  begin
    while bufIdx < aReceivedCount do      // Loop until all messages are extracted from the buffer
    begin            
      if aConnection^.msgLen > 0 then     // Size of the message is based on the data in the connection, or from packet
      begin
        msg := aConnection^.partMsgData;
        msgLen := aConnection^.msgLen - Length(msg);  // Adjusted length for previous read
        aConnection^.msgLen := -1;
        aConnection^.partMsgData := '';
      end
      else
      begin
        msg := '';
        msgLen := Byte(buffer[bufIdx]);    // Get the length of the next message
        // WriteLn('msglen: ', msgLen);
        bufIdx += 1;
      end;
      
      for bufIdx := bufIdx to bufIdx + msgLen - 1 do
      begin
        if (bufIdx >= aReceivedCount) or (bufIdx > Length(buffer)) then 
        begin
          aConnection^.partMsgData := msg;
          aConnection^.msgLen      := msgLen;
          // WriteLn('Message: ', msg, ' ');
          // WriteLn('Part message: ', msg);
          exit;                           // Exit... end of buffer, but not end of message
        end;
        
        msg += buffer[bufIdx];
      end;
      
      EnqueueMessage(msg, aConnection);
      // WriteLn('Receive message: ', msg, ' ');
      
      bufIdx += 1;                        // Advance to start of next message
    end;
  end;
    
  // function TCPIP(aNewSocket : PTCPSocket) : LongWord;
  // var
  //   lRemoteIP : PIPAddress;
  // begin
  //   lRemoteIP := SDLNet_TCP_GetPeerAddress(aNewSocket);
  //   result := SDLNet_Read32(@lRemoteIP^.host);
  // end;

  // function TCPPort(aNewSocket : PTCPSocket) : LongInt;
  // var
  //   lRemoteIP : PIPAddress;
  // begin
  //   lRemoteIP := SDLNet_TCP_GetPeerAddress(aNewSocket);
  //   result := SDLNet_Read16(@lRemoteIP^.port);
  // end;
  
//----------------------------------------------------------------------------
// Misc Function
//----------------------------------------------------------------------------

  function MyIPProcedure() : String;
  begin
    result := '127.0.0.1';
  end;
  
  
//----------------------------------------------------------------------------
// TCP Connection Handling
//----------------------------------------------------------------------------

  function OpenTCPConnection(const host: String; port: LongInt) : Connection;
  var
    con : psg_network_connection;
  begin
    result := nil;

    New(con);
    con^ := _sg_functions^.network.open_tcp_connection(PChar(host), port);
    // WriteLn('client con = ', HexStr(con^._socket), ' ', con^.kind);
    if Assigned(con^._socket) and (con^.kind = SGCK_TCP) then
    begin
      result := CreateConnection();
      result^.stringIP      := host;
      result^.IP            := _sg_functions^.network.network_address(con);
      result^.port          := port;
      result^.socket        := con;
      result^.protocol      := TCP;
      
      SetLength(_Connections, Length(_Connections) + 1);
      _Connections[High(_Connections)] := result;
      
      // SDLNet_AddSocket(_SocketSet, PSDLNet_GenericSocket(lTempSocket));
    end;
  end;  

  function CreateTCPServer(const name: String; port : LongInt) : ServerSocket;
  var
    con : psg_network_connection;
  begin
    result := nil;
    New(con);
    con^ := _sg_functions^.network.open_tcp_connection(nil, port);

    // WriteLn('svr con = ', HexStr(con^._socket), ' ', con^.kind);

    if Assigned(con^._socket) and (con^.kind = SGCK_TCP) then
    begin
      New(result);
      result^.name := name;
      result^.socket := con;
      result^.port := port;
      result^.newConnections := 0;
      result^.protocol := TCP;
      SetLength(result^.connections, 0);

      SetLength(_Servers, Length(_Servers) + 1);
      _Servers[High(_Servers)] := result;
    end
    else
    begin
      FreeAndNil(con);
    end;
  end;

  function AcceptAllNewConnections () : LongInt;
  var
    i: Integer;
  begin
    result := 0;
    for i := 0 to High(_Servers) do
    begin
      result += AcceptNewConnection(_Servers[i]);
    end;
  end;

  function AcceptNewConnection(server: ServerSocket) : LongInt;
  var
    con : sg_network_connection;
    conP: psg_network_connection;
    client: Connection;
  //   lTempSocket : PTCPSocket = nil;
  //   lNewConnection : Connection;
  //   i : LongInt;
  begin  
    result := 0;
    server^.newConnections := 0;
    con := _sg_functions^.network.accept_new_connection(server^.socket);

    // WriteLn('svr client con = ', HexStr(con._socket), ' ', con.kind);

    if Assigned(con._socket) and (con.kind = SGCK_TCP) then
    begin
      client := CreateConnection();
      client^.IP := _sg_functions^.network.network_address(@con);
      client^.port := server^.port;
      New(conP);
      conP^ := con;
      client^.socket := conP;

      SetLength(server^.connections, Length(server^.connections) + 1);
      server^.connections[High(server^.connections)] := client;
      server^.newConnections := 1;
      result := 1;
    end;

    // for i := Low(_ListenSockets) to High(_ListenSockets) do
    // begin
    //   lTempSocket := SDLNet_TCP_Accept(_ListenSockets[i].socket);
    //   if Assigned(lTempSocket) then
    //   begin
    //     lNewConnection                := CreateConnection();
    //     lNewConnection^.IP            := _sg_functions^.network.network_address(con);
    //     lNewConnection^.socket        := lTempSocket;
    //     lNewConnection^.port          := _ListenSockets[i].port;
    //     SetLength(_Connections, Length(_Connections) + 1);
    //     _Connections[High(_Connections)] := lNewConnection;
    //     SDLNet_AddSocket(_SocketSet, PSDLNet_GenericSocket(lTempSocket));
    //     EnqueueNewConnection(lNewConnection);
    //     result += 1;
    //   end;
    // end;
  end;

//----------------------------------------------------------------------------
// TCP Message Handling
//----------------------------------------------------------------------------

  procedure CheckNetworkActivity();
  var
    svr, i: Integer;
  begin
    // check if there is data on the network
    if _sg_functions^.network.network_has_data() > 0 then
    begin
      WriteLn('should be some data...');
      for svr := 0 to High(_servers) do
      begin
        for i := 0 to High(_servers[svr]^.connections) do
        begin
          if _sg_functions^.network.connection_has_data(_servers[svr]^.connections[i]^.socket) > 0 then
          begin
            WriteLn('Data for connection: ', i);
          end;
        end;
      end;
    end;
  end;

  function MessagesReceived() : Boolean;
  // var
  //   i, lReceived : LongInt;
  //   buffer: PacketData;
  begin
    result := False;
    // if SDLNet_CheckSockets(_SocketSet, 0) < 1 then exit;
    // for i := Low(_Connections) to High(_Connections) do
    // begin
    //   if SDLNET_SocketReady(PSDLNet_GenericSocket(_Connections[i]^.socket)) then
    //   begin
    //      if (_Connections[i]^.protocol = TCP) then
    //      begin
    //        lReceived := SDLNet_TCP_Recv(_Connections[i]^.socket, @buffer, 512);
    //        if (lReceived <= 0) then continue;
    //        ExtractData(buffer, lReceived, _Connections[i])
    //      end
    //      else if (_Connections[i]^.protocol = Http) then
    //      begin
    //        WriteLn('ERROR -- ExtractHttpData');
    //        // ExtractHttpData(_Connections[i]);
    //      end;
    //     result := True;
    //   end;
    // end;
  end;

  function SendHttpRequestProcedure(const aReq: HttpRequest; const aConnection : Connection) : Connection;
  // var
  //   lLen, i : LongInt;
  //   buffer : Array of Char;
  //   lMsg : String = '';
  begin
    result := nil;
    // if (aConnection = nil) or (aConnection^.socket = nil) or (aConnection^.protocol <> Http) then 
    // begin 
    //   RaiseWarning('SDL 1.2 SendTCPMessageProcedure Illegal Connection Arguement (nil or not Http)'); 
    //   exit; 
    // end;

    // lMsg := HttpRequestToString(aReq) + #13#10#13#10;
    // SetLength(buffer, Length(lMsg));

    // for i := 0 to Length(lMsg) - 1 do
    //   buffer[i] := lMsg[i + 1];
    
    // lLen := Length(lMsg);
    
    // // WriteLn(lMsg);

    // if (SDLNet_TCP_Send(aConnection^.socket, @buffer[0], lLen) < lLen) then
    // begin
    //       result := aConnection;
    //       RaiseWarning('Error sending message: SDLNet_TCP_Send: ' + SDLNet_GetError() + ' Http Connection may have been refused.');
    // end;
  end;

  function SendMessageTo(const aMsg : String; aConnection: Connection) : Boolean;
  var
    len, i : LongInt;
    buffer: PacketData;
  begin
    result := false;

    if (aConnection = nil) or (aConnection^.socket = nil) then begin RaiseWarning('SendMessageTo Missing Connection, or connection closed'); exit; end;
    if Length(aMsg) > 255 then begin RaiseWarning('SendMessageTo: SwinGame messages must be less than 256 characters in length'); exit; end;

    len := Length((aMsg)) + 1;

    for i := 0 to Length(aMsg) do
    begin
      if i = 0 then
        buffer[i] := Char(Length(aMsg))
      else
        buffer[i] := aMsg[i]; // 1 to Length
    end;

    if _sg_functions^.network.send_bytes(aConnection^.socket, @buffer[0], len) = len then
    begin
      result := true;
    end;
  end;

  procedure BroadcastTCPMessageProcedure(const aMsg : String);
  // var
  //   lLen, i : LongInt;
  //   buffer: PacketData;
  begin
    // if Length(aMsg) > 255 then begin RaiseWarning('SwinGame messages must be less than 256 characters in length'); exit; end;
      
    // for i := 0 to Length(aMsg) + 1 do
    // begin
    //   if i = 0 then
    //     buffer[i] := Char(Length(aMsg))
    //   else if  i < Length(aMsg) + 1 then
    //     buffer[i] := aMsg[i];
    // end;
    
    // lLen := Length(aMsg) + 1;
    // for i := Low(_Connections) to High(_Connections) do
    // begin
    //   if (SDLNet_TCP_Send(_Connections[i]^.socket, @buffer[0], lLen) < lLen) then
    //   begin
    //     RaiseWarning('Error broadcasting message: SDLNet_TCP_Send: ' + SDLNet_GetError());
    //   end;
    // end;
  end;

//----------------------------------------------------------------------------
// UDP Connections
//----------------------------------------------------------------------------

  procedure CreatePackets();
  begin
    // if _UDPSendPacket = nil then
    //   _UDPSendPacket := SDLNet_AllocPacket(512);
    // if _UDPReceivePacket = nil then
    //   _UDPReceivePacket := SDLNet_AllocPacket(512);
  end;

  function CreateUDPHostProcedure(const aPort : LongInt) : LongInt;
  // var
  //   lTempSocket  : PUDPSocket = nil;  
  //   lPortID      : String;  
  begin    
    // lPortID := IntToStr(aPort);
    result := -1;
    // if HasName(_UDPSocketIDs, lPortID) then begin result := IndexOf(_UDPSocketIDs, lPortID); exit; end;
    // lTempSocket := SDLNet_UDP_Open(aPort);
    // if Assigned(lTempSocket) then
    // begin
    //   SetLength(_UDPListenSockets, Length(_UDPListenSockets) + 1);
    //   _UDPListenSockets[High(_UDPListenSockets)] := lTempSocket;
    //   AddName(_UDPSocketIDs, lPortID);
    //   result := High(_UDPListenSockets);
    // end;
    // CreatePackets();
    // if result = -1 then RaiseWarning('OpenUDPListenerPort: ' + SDLNET_GetError());
  end;
   
  function CreateUDPConnectionProcedure(const aDestIP : String; const aDestPort, aInPort : LongInt) : Connection; 
  // var
  //   lIdx : LongInt;
  //   lDecDestIP : LongWord;
  //   lDecIPStr : String;
  begin    
    result := nil;
    // lDecDestIP := IPv4ToDec(aDestIP);
    // lDecIPStr  := IntToStr(lDecDestIP);

    // if HasName(_UDPConnectionIDs, lDecIPStr + ':' + IntToStr(aDestPort)) then exit;
    // lIdx := CreateUDPHostProcedure(aInPort);

    // if (lIdx = -1) then begin RaiseWarning('SDL 1.2 - CreateUDPConnectionProcedure: Could not Bind Socket.'); exit; end;

    // AddName(_UDPConnectionIDs, lDecIPStr + ':' + IntToStr(aDestPort));
    // result := CreateConnection();
    // result^.ip := lDecDestIP;
    // result^.port := aDestPort;
    // result^.protocol := UDP;
    // result^.stringIP := lDecIPStr;
    
    // lIdx := CreateUDPHostProcedure(aInPort);
    // result^.socket := _UDPListenSockets[lIdx];
    // SetLength(_Connections, Length(_Connections) + 1);
    // _Connections[High(_Connections)] := result;
    // CreatePackets();
    // if not Assigned(result^.socket) then RaiseWarning('OpenUDPSendPort: ' + SDLNET_GetError());
  end;

//----------------------------------------------------------------------------
// UDP Message
//----------------------------------------------------------------------------

  function UDPMessageReceivedProcedure() : Boolean;
  // var
  //   i, j          : LongInt;    
  //   lMsg          : String = '';
  //   lConnection   : Connection;
  //   lSrcIPString  : String;
  //   lNewConnection: Boolean = False;
  //   lSrcPort : LongInt;
  //   lSrcIP  : LongWord;
  begin
    result := False;
    // for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
    // begin
    //   if SDLNet_UDP_Recv(_UDPListenSockets[i], _UDPReceivePacket) > 0 then
    //   begin        
    //     lSrcIP      := SDLNet_Read32(@_UDPReceivePacket^.address.host);
    //     lSrcPort    := SDLNet_Read16(@_UDPReceivePacket^.address.port);
    //     lConnection := GetConnectionWithID(lSrcIP, lSrcPort, UDP);

    //     if not Assigned(lConnection) then
    //     begin
    //       lSrcIPString := HexStrToIPv4(DecToHex(lSrcIP));
    //       lConnection := CreateUDPConnectionProcedure(lSrcIPString, lSrcPort, StrToInt(NameAt(_UDPSocketIDs, i)));
    //       lNewConnection := True;
    //     end;
        
    //     if not Assigned(lConnection) then begin RaiseWarning('SDL 1.2 - UDPMessageReceivedProcedure: Could Not Create Connection.'); exit; end;

    //     for j := 0 to _UDPReceivePacket^.len - 1 do
    //       lMsg += Char((_UDPReceivePacket^.data)[j]);
        
    //     if lNewConnection then
    //       EnqueueNewConnection(lConnection);

    //     EnqueueMessage(lMsg, lConnection);
    //     result := True;
    //   end; 
    // end;
  end;
    
  function SendUDPMessageProcedure(const aMsg : String; const aConnection : Connection) : Boolean;
  // var
  //   lIPAddress : TIPaddress;
  begin
    result := False;
    // if not Assigned(aConnection) then begin RaiseWarning('SDL 1.2 - SendUDPMessageProcedure: Unassigned Connection.'); exit; end;
    // SDLNet_ResolveHost(lIPAddress, PChar(HexStrToIPv4(DecToHex(aConnection^.ip))), aConnection^.port);

    // _UDPSendPacket^.address.host   := lIPAddress.host;
    // _UDPSendPacket^.address.port   := lIPAddress.port;
    // _UDPSendPacket^.len            := Length(aMsg);
    // _UDPSendPacket^.data           := @(aMsg[1]);
    // SDLNet_UDP_Send(aConnection^.socket, -1, _UDPSendPacket);
    // result := True; 
  end;
  
  procedure BroadcastUDPMessageProcedure(const aMsg : String);
  // var
  //   lIPAddress : TIPaddress;
  //   i : LongInt;
  begin
    // for i := Low(_Connections) to High(_Connections) do
    // begin
    //   SDLNet_ResolveHost(lIPAddress, PChar(HexStrToIPv4(DecToHex(_Connections[i]^.ip))), _Connections[i]^.port);

    //   _UDPSendPacket^.address.host   := lIPAddress.host;
    //   _UDPSendPacket^.address.port   := lIPAddress.port;
    //   _UDPSendPacket^.len            := Length(aMsg);
    //   _UDPSendPacket^.data           := @(aMsg[1]);
    //   SDLNet_UDP_Send(_Connections[i]^.socket, -1, _UDPSendPacket);
    // end;
  end;

//----------------------------------------------------------------------------
// Close Single
//----------------------------------------------------------------------------

  // function CloseUDPSocket(var aSocket : PUDPSocket) : Boolean;
  // // var
  // //   lTmpSockets   : Array of PUDPSocket;
  // //   i, j, lOffset : LongInt;
  // begin
  //   result := False;
  //   // if (Length(_UDPListenSockets) = 0) or not Assigned(aSocket) then begin RaiseWarning('SDL 1.2 - CloseUDPListenSocketProcedure: Could Not Close UDP Socket.'); exit; end;
    
  //   // lOffset := 0;
  //   // SetLength(lTmpSockets, Length(_UDPListenSockets) - 1);
  //   // for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
  //   // begin
  //   //   if aSocket = _UDPListenSockets[i] then 
  //   //   begin
  //   //     lOffset := 1;
  //   //     for j := Low(_Connections) to High(_Connections) do
  //   //       if _Connections[j]^.socket = _UDPListenSockets[i] then
  //   //         _Connections[j]^.socket := nil;
  //   //     SDLNet_UDP_Close(_UDPListenSockets[i]);
  //   //     RemoveName(_UDPSocketIDs, i);
  //   //   end else
  //   //     lTmpSockets[i - lOffset] := _UDPListenSockets[i];
  //   // end;
  //   // _UDPListenSockets := lTmpSockets; 
  //   // result := True;
  // end;
  
  function CloseTCPHostSocketProcedure(const aPort : LongInt) : Boolean;
  // var
  //   lTmpListenArray : Array of TCPListenSocket;
  //   offSet: LongInt = 0;
  //   i   : LongInt;
  begin
    result := False;
    // if Length(_ListenSockets) = 0 then begin RaiseWarning('SDL 1.2 - CloseUDPListenSocketProcedure: Could Not Close TCP Socket.'); exit; end;
    // SetLength(lTmpListenArray, Length(_ListenSockets));
    // for i := Low(_ListenSockets) to High(_ListenSockets) do
    // begin
    //   if (_ListenSockets[i].port = aPort) then
    //   begin
    //     offSet := 1;
    //     if Assigned(_ListenSockets[i].socket) then 
    //     begin
    //       SDLNet_TCP_Close(_ListenSockets[i].socket);
    //       result := True;
    //     end;
    //   end else
    //     lTmpListenArray[i - offset] := _ListenSockets[i];
    // end;
    // if (result) then
    // begin
    //   _ListenSockets := lTmpListenArray; 
    //   SetLength(_ListenSockets, Length(_ListenSockets) - 1);
    // end;
  end;

  function CloseConnectionProcedure(var aConnection : Connection; const aCloseUDPSocket : Boolean) : Boolean;
  // var
  //   lTmpConnectionArray : ConnectionArray;
  //   offSet: LongInt = 0;
  //   i : LongInt;
  begin
    result := False;
    // if (Length(_Connections) = 0) or (not Assigned(aConnection)) then begin RaiseWarning('SDL 1.2 - CloseConnectionProcedure: Could Not Close Connection.'); exit; end;
    // SetLength(lTmpConnectionArray, Length(_Connections) - 1);
    // for i := Low(_Connections) to High(_Connections) do
    // begin
    //   if (_Connections[i] = aConnection) then
    //   begin
    //     offSet := 1;
    //     if (aConnection^.protocol <> UDP) and Assigned(aConnection^.socket) then 
    //     begin
    //       SDLNet_TCP_DelSocket(_Socketset, aConnection^.socket);
    //       SDLNet_TCP_Close(aConnection^.socket);
    //     end else if (aConnection^.protocol = UDP) then
    //       RemoveName(_UDPConnectionIDs, IntToStr(aConnection^.ip) + ':' + IntToStr(aConnection^.port));
    //     result := True;
    //   end else begin
    //     lTmpConnectionArray[i - offset] := _Connections[i];
    //   end;
    // end;    
    // if aCloseUDPSocket and Assigned(aConnection^.socket) then 
    //   CloseUDPSocket(aConnection^.socket);
    // ClearMessageQueue(aConnection);
    // FreeConnection(aConnection);
    // if (result) then
    //   _Connections := lTmpConnectionArray;
  end;
 
  function CloseUDPSocketProcedure(const aPort : LongInt) : Boolean;
  // var
  //   lTmpSockets : Array of PUDPSocket;
  //   i, j, lOffset, lIdx  : LongInt;
  begin
    result := False;
    // if Length(_UDPListenSockets) = 0 then begin RaiseWarning('SDL 1.2 - CloseUDPListenSocketProcedure: Could Not Close UDP Socket.'); exit; end;
    // lIdx := IndexOf(_UDPSocketIDs, IntToStr(aPort));
    // if lIdx = -1 then exit;
    // RemoveName(_UDPSocketIDs, lIdx);
    // lOffset := 0;
    // SetLength(lTmpSockets, Length(_UDPListenSockets) - 1);
    // for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
    // begin
    //   if i = lIdx then 
    //   begin
    //     lOffset := 1;
    //     for j := Low(_Connections) to High(_Connections) do
    //       if _Connections[j]^.socket = _UDPListenSockets[i] then
    //         _Connections[j]^.socket := nil;
    //     SDLNet_UDP_Close(_UDPListenSockets[i])
    //   end else
    //     lTmpSockets[i - lOffset] := _UDPListenSockets[i];
    // end;
    // _UDPListenSockets := lTmpSockets; 
    // result := True;
  end;
  
//----------------------------------------------------------------------------
// Close All
//----------------------------------------------------------------------------

  procedure CloseAllTCPHostSocketProcedure();
  // var
  //   i : LongInt;
  begin    
    // for i := Low(_ListenSockets) to High(_ListenSockets) do
    //   SDLNet_TCP_Close(_ListenSockets[i].socket);
    // SetLength(_ListenSockets, 0);
  end;

  procedure CloseAllConnectionsProcedure(const aCloseUDPSockets : Boolean);
  begin
   //  while (Length(_Connections) <> 0) do
   //    CloseConnectionProcedure(_Connections[High(_Connections)], aCloseUDPSockets);
   // { for i := Low(_Connections) to High(_Connections) do
   //  begin
   //    if (_Connections[i]^.isTCP) and Assigned(_Connections[i]^.socket) then 
   //    begin
   //      SDLNet_DelSocket(_SocketSet, _Connections[i]^.socket);
   //      SDLNet_TCP_Close(_Connections[i]^.socket);
   //    end else begin
   //      RemoveAllNamesInCollection(_UDPConnectionIDs);
   //      if Assigned(_Connections[i]^.socket) and aCloseUDPSocket then
   //        SDLNet_UDP_Close(_Connections[i]^.socket);
   //    end;
   //    ClearMessageQueue(_Connections[i]);
   //    FreeConnection(_Connections[i]);
   //  end;
   //  SetLength(_Connections, 0);}
  end;

  procedure CloseAllUDPSocketProcedure();
  // var
  //   i : LongInt;
  begin
    // for i := Low(_UDPListenSockets) to High(_UDPListenSockets) do
    //   SDLNet_UDP_Close(_UDPListenSockets[i]);
    // SetLength(_UDPListenSockets, 0);
  end;  

  procedure FreeAllNetworkingResourcesProcedure();
  begin
    CloseAllConnectionsProcedure(False);
    CloseAllTCPHostSocketProcedure();
    CloseAllUDPSocketProcedure();
      
 //    if Assigned(_UDPReceivePacket) then
 //    begin
 //      SDLNet_FreePacket(_UDPReceivePacket);
 //      _UDPReceivePacket := nil;
 //    end;
 //    if Assigned(_SocketSet) then
 //    begin
 //      SDLNet_FreeSocketSet(_SocketSet);
 //      _SocketSet := nil;
 //    end;
 // //   _UDPSendPacket is not Allocated
 // //   if Assigned(_UDPSendPacket) then
 // //     SDLNet_FreePacket(_UDPSendPacket);   
 //    FreeNamedIndexCollection(_UDPSocketIDs);
 //    FreeNamedIndexCollection(_UDPConnectionIDs);
  end;


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
      
  procedure ReconnectConnection(var aConnection : Connection);
  var
    lIPString : String;
    lPort : LongInt;
  begin
    lIPString := aConnection^.stringIP;
    lPort := aConnection^.port;

    CloseConnection(aConnection);
    aConnection := NetworkingDriver.CreateTCPConnection(lIPString, lPort, Http);
  end;
  
  function AcceptTCPConnection() : LongInt;
  begin
    result := NetworkingDriver.AcceptTCPConnection();
  end;
  
//----------------------------------------------------------------------------
// Connection
//----------------------------------------------------------------------------

  function ConnectionCount(server: ServerSocket) : LongInt;
  begin
    result := 0;
    if (not Assigned(server)) then exit;
    result := Length(server^.connections);
  end;

  function RetreiveConnection(server: ServerSocket; idx: LongInt) : Connection;
  begin
    result := nil;
    if (not Assigned(server)) or (idx < 0) or (idx > High(server^.connections)) then exit;
    result := server^.connections[idx];
  end;

  function LastConnection(server: ServerSocket) : Connection;
  begin
    result := nil;
    if (not Assigned(server)) or (Length(server^.connections) <= 0) then exit;
    result := server^.connections[High(server^.connections)];
  end;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------
  
  procedure EnqueueMessage( const aMsg : String; aConnection : Connection);
  var
    msgData   : MessagePtr;
  begin
    if not Assigned(aConnection) then exit;

    New(msgData); 
    msgData^.data := aMsg;
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

  function HttpResponseBodyAsString(httpData: HttpResponse): String;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to High(httpData.body) do
    begin
      result += Char(httpData.body[i]);
    end;
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

  procedure BroadcastTCPMessage(aMsg : String);
  begin
    NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;
  
//----------------------------------------------------------------------------
// Http
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

  function CreateHttpConnection(const aDestIP : String; const aDestPort : LongInt) : Connection;
  begin
    result := NetworkingDriver.CreateTCPConnection(aDestIP, aDestPort, Http);
  end;
  
  procedure SendHttpRequest(const aReq : HttpRequest; const aConnection : sg_network_connection);
  var
    lLen, i : LongInt;
    buffer : Array of Char;
    lMsg : String = '';
  begin
    // result := nil;
    // if (aConnection = nil) or (aConnection^.socket = nil) or (aConnection^.protocol <> Http) then 
    // begin 
    //   RaiseWarning('SDL 1.2 SendTCPMessageProcedure Illegal Connection Arguement (nil or not Http)'); 
    //   exit; 
    // end;

    lMsg := HttpRequestToString(aReq) + #13#10#13#10;
    SetLength(buffer, Length(lMsg));

    for i := 0 to Length(lMsg) - 1 do
      buffer[i] := lMsg[i + 1];
    
    lLen := Length(lMsg);
    
    // WriteLn(lMsg);

    if _sg_functions^.network.send_bytes(@aConnection, @buffer[0], lLen) < lLen then
    // if (SDLNet_TCP_Send(aConnection^.socket, @buffer[0], lLen) < lLen) then
    begin
      // RaiseWarning('Error sending message: SDLNet_TCP_Send: ' + SDLNet_GetError() + ' Http Connection may have been refused.');
    end;
  end;

  procedure HttpAddHeader(var aHttpRequest : HttpRequest; const name, value : String);
  begin
    SetLength(aHttpRequest.headername, Length(aHttpRequest.headername) + 1);
    SetLength(aHttpRequest.headervalue, Length(aHttpRequest.headervalue) + 1);
    aHttpRequest.headername[High(aHttpRequest.headername)] := name;
    aHttpRequest.headervalue[High(aHttpRequest.headervalue)] := value;
  end;

  procedure HttpRemoveHeaderAt(var aHttpRequest : HttpRequest; const aIdx : LongInt);
  var
    i : Integer;
  begin
    for i := aIdx to High(aHttpRequest.headername) do 
    begin
      if i = High(aHttpRequest.headername) then continue;

      aHttpRequest.headername[i] := aHttpRequest.headername[i + 1];
      aHttpRequest.headervalue[i] := aHttpRequest.headervalue[i + 1];
    end;

    SetLength(aHttpRequest.headername, Length(aHttpRequest.headername) - 1);
    SetLength(aHttpRequest.headervalue, Length(aHttpRequest.headervalue) - 1);
  end;

  function HttpHeaderAt(const aHttpRequest : HttpRequest; const aIdx : LongInt) : String;
  begin
    result := '';
    if (aIdx < 0) or (aIdx > High(aHttpRequest.headername)) then exit;

    result := aHttpRequest.headername[aIdx] + ': '+ aHttpRequest.headervalue[aIdx];
  end;

  procedure HttpSetBody(var aHttpRequest : HttpRequest; const aBody : String);
  begin
    aHttpRequest.body := aBody;
  end;

  procedure HttpSetMethod(var aHttpRequest : HttpRequest; const aMethod : HttpMethod);
  begin
    aHttpRequest.requestType := aMethod;
  end;

  procedure HttpSetVersion(var aHttpRequest : HttpRequest; const aVersion : String);
  begin
    aHttpRequest.version := aVersion;
  end;

  procedure HttpSetURL(var aHttpRequest : HttpRequest; const aURL : String);
  begin
    aHttpRequest.url := aURL;
  end;

  function HttpRequestToString(const aHttpRequest : HttpRequest) : String;
  var
    i, len : Integer;
  begin
    result := '';
    case aHttpRequest.requestType of
      Http_GET: result += 'GET ';
      Http_POST: result += 'POST ';
      Http_PUT: result += 'PUT ';
      Http_DELETE: result += 'DELETE ';
    end;
    result += aHttpRequest.url;
    result += ' Http/' + aHttpRequest.version;
    result += #13#10;
    for i := Low(aHttpRequest.headername) to High(aHttpRequest.headername) do
      result += aHttpRequest.headername[i] + ': ' + aHttpRequest.headervalue[i] + #13#10;
    len := Length(aHttpRequest.body);

    if len <> 0 then
      result += 'Content-Length: ' + IntToStr(len) + #13#10#13#10;
    result += aHttpRequest.body;
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
      result.protocol := 'Http/1.1';
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

  function HttpGet(host: String; port: LongInt; path: String) : HttpResponse;
  var
    con : sg_network_connection;
    request : HttpRequest;
  begin
    // ip := HostIP(host);
    // ip := host;
    HttpAddHeader(request, 'Host', host + ':' + IntToStr(port));
    HttpAddHeader(request, 'Connection', 'close');

    // Create Http message
    HttpSetMethod(request, Http_GET);
    HttpSetURL(request, path);
    HttpSetVersion(request, '1.1');

    HttpSetBody(request, '');
    
    con := _sg_functions^.network.open_tcp_connection(PChar(host), port);

    // CreateHttpConnection(ip, port);
    SendHttpRequest(request, con);
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

    // for i := 0 to _NewConnectionCount - 1 do
    //   FetchConnection();
    // _NewConnectionCount := 0;
  end;

//----------------------------------------------------------------------------
// Other
//----------------------------------------------------------------------------

  function MyIP() : String;
  begin
    result := NetworkingDriver.MyIP();
  end;

//=============================================================================

  initialization 
  begin
    // _SocketSet := SDLNET_AllocSocketSet(16);
    // InitNamedIndexCollection(_UDPSocketIDs);
    // InitNamedIndexCollection(_UDPConnectionIDs);
  end;

  finalization
  begin
    FreeAllNetworkingResourcesProcedure();
  end;
end.