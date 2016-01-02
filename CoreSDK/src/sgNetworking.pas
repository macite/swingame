//=============================================================================
// sgNetworking.pas
//=============================================================================


/// The networking code of SwinGame is used for TCP and UDP connections to
/// and from multiple clients.
/// 
/// @module Networking
/// @static
unit sgNetworking;
interface
uses sgTypes;


//----------------------------------------------------------------------------
// Server code
//----------------------------------------------------------------------------

  /// Creates a server socket that listens for TCP connections 
  /// on the port given. Returns the server if this succeeds, otherwise
  /// it returns nil/null.
  ///
  /// @param name The name of the server to allow it to be accessed by name
  /// @param port The number of the port to listen to for new connections
  ///
  /// @lib
  /// @sn createServerNamed:%s onPort:%s
  function CreateServer(const name: String; port: Word) : ServerSocket;

  /// Creates a server socket that listens for connections 
  /// on the port given. Returns the server if this succeeds, otherwise
  /// it returns nil/null.
  ///
  /// @param name The name of the server to allow it to be accessed by name
  /// @param port The number of the port to listen to for new connections
  /// @param protocol The kind of server to create -- TCP or UDP
  ///
  /// @lib CreateServerWithProtocol
  /// @sn createServerNamed:%s onPort:%s withProtocol:%s
  function CreateServer(const name: String; port: Word; protocol: ConnectionType) : ServerSocket;

  /// Returns the Server socket for the give name, or nil/null if there is no
  /// server with that name.
  ///
  /// @lib
  function ServerNamed(const name: String): ServerSocket;

  /// Indicates if there is a new connection to a server.
  ///
  /// @lib
  function ServerHasNewConnection(server: ServerSocket) : Boolean;

  /// Indicates if there is a new connection to a server.
  ///
  /// @lib ServerNamedHasNewConnection
  function ServerHasNewConnection(const name: String) : Boolean;

  /// Indicates if there is a new connection to any of the servers
  /// that are currently listening for new clients.
  ///
  /// @lib
  function HasNewConnections() : Boolean;


//----------------------------------------------------------------------------
// Client code
//----------------------------------------------------------------------------

  /// Opens a connection to a server using the IP and port.
  /// Creates a Connection for the purpose of two way messages. 
  /// Returns a new connection if successful or nil/null if it fails.
  ///
  /// @param host The IP Address or domain name of the host
  /// @param port The port number the server is using to listen for connections
  ///
  /// @lib
  /// @sn openConnection:%s port:%s
  function OpenConnection(const host: String; port: Word) : Connection;

  /// Opens a connection to a server using the IP and port.
  /// Creates a Connection for the purpose of two way messages. 
  /// Returns a new connection if successful or nil/null if it fails.
  /// This version allows you to name the connection, so that you can
  /// access it via its name.
  ///
  /// @param name The name of the connection, used to access it in networking calls
  /// @param host The IP Address or domain name of the host
  /// @param port The port number the server is using to listen for connections
  ///
  /// @lib OpenConnectionNamed
  /// @sn openConnectionNamed:%s toHost:%s port:%s
  function OpenConnection(const name, host: String; port: Word) : Connection;

  /// Opens a connection to a server using the IP and port.
  /// Creates a Connection for the purpose of two way messages. 
  /// Returns a new connection if successful or nil/null if it fails.
  /// This version allows you to name the connection, so that you can
  /// access it via its name.
  ///
  /// @param name The name of the connection, used to access it in networking calls
  /// @param host The IP Address or domain name of the host
  /// @param port The port number the server is using to listen for connections
  /// @param protocol The kind of connection to make (TCP or UDP)
  ///
  /// @lib OpenConnectionNamedWithProtocol
  /// @sn openConnectionNamed:%s toHost:%s port:%s withProtocol:%s
  function OpenConnection(const name, host: String; port: Word; protocol: ConnectionType) : Connection;

  /// You can use this to check if a connection is currently open.
  /// A connection may be closed by the remote machine.
  ///
  /// @lib
  function ConnectionOpen(con: Connection) : Boolean;

  /// You can use this to check if a connection is currently open.
  /// A connection may be closed by the remote machine.
  ///
  /// @lib ConnectionOpenNamed
  function ConnectionOpen(const name: String) : Boolean;

  /// Returns the connection for the give name, or nil/null if there is no
  /// connection with that name.
  ///
  /// @lib
  function ConnectionNamed(const name: String): Connection;

  /// Attempts to recconnect a connection that was closed using the IP and port
  /// stored in the connection
  ///
  /// @lib
  /// @class Connection
  /// @method Reconnect
  procedure Reconnect(aConnection : Connection);

  /// Attempts to recconnect a connection that was closed using the IP and port
  /// stored in the connection. Finds the connection using its name.
  ///
  /// @lib ReconnectConnectionNamed
  procedure Reconnect(const name: String);
   
  /// Broadcasts a message to all connections (all servers and opened connections).
  ///
  /// @lib
  procedure BroadcastMessage(const aMsg : String);

  /// Broadcasts a message to all connections to a given server.
  ///
  /// @lib BroadcastMessageToServer
  /// @sn broadcastMessage: %s toServer:%s
  procedure BroadcastMessage(const aMsg : String; svr: ServerSocket);

  /// Broadcasts a message to all connections to a given server.
  ///
  /// @lib BroadcastMessageToServerNamed
  /// @sn broadcastMessage: %s toServerNamed:%s
  procedure BroadcastMessage(const aMsg : String; const name: String);

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

  /// Sends the message over the provided network connection (found from its name).
  /// Returns true if this succeeds, or false if it fails.
  ///
  /// @param aMsg The message to be sent
  /// @param name The name of the connection to send the message over
  ///
  /// @lib SendMessageToConnectionNamed
  /// @sn sendMessage:%s toConnectionNamed:%s
  function SendMessageTo(const aMsg, name: String) : Boolean;

  /// If you access a message directly, you need to make sure that it is closed when
  /// you have finished with its details. This procedure frees the resources
  /// used by the message.
  ///
  /// @lib
  ///
  /// @class Message
  /// @dispose
  procedure CloseMessage(msg: Message);

  /// This procedure checks for any network activity.
  /// It first check all servers for incomming connections from clients,
  /// then it checks for any messages received over any of 
  /// the connections SwinGame is managing.
  ///
  /// @lib
  procedure CheckNetworkActivity();

//----------------------------------------------------------------------------
// Misc
//----------------------------------------------------------------------------

  /// Returns the number of connections to a Server socket.
  ///
  /// @lib
  function ConnectionCount(server: ServerSocket) : LongInt;

  /// Returns the number of connections to a Server socket.
  ///
  /// @lib ConnectionCountForServerNamed
  function ConnectionCount(const name: String) : LongInt;
  
  /// Retrieves the connection at the specified index
  ///
  /// @param idx The index of the connection
  ///
  /// @lib
  function RetreiveConnection(server: ServerSocket; idx: LongInt) : Connection;

  /// Retrieves the connection at the specified index
  ///
  /// @param idx The index of the connection
  ///
  /// @lib RetrieveConnectionFromServerNamed
  function RetreiveConnection(const name: String; idx: LongInt) : Connection;

  /// Returns the last connection made to a server socket. When a new client 
  /// has connected to the server, this function can be used to get their
  /// connection.
  ///
  /// @lib
  function LastConnection(server: ServerSocket) : Connection;

  /// Returns the last connection made to a server socket. When a new client 
  /// has connected to the server, this function can be used to get their
  /// connection.
  ///
  /// @lib LastConnectionForServerNamed
  function LastConnection(const name: String) : Connection;

//----------------------------------------------------------------------------
// Messages and Connection Data Access
//----------------------------------------------------------------------------

  /// Gets the IP address (an number) of the destination for the connection.
  ///
  /// @param aConnection The connection to get the ip address from
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method IpAddress
  function  ConnectionIP(aConnection : Connection) : LongWord;

  /// Gets the IP address (an number) of the destination for the connection (found by its name).
  ///
  /// @param name The name of the connection to get the ip address from
  ///
  /// @lib ConnectionIPNamed
  function  ConnectionIP(const name: String) : LongWord;

  /// Gets the Port of the destination for the connectiom
  ///
  /// @param aConnection The connection to get the port from
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method Port
  function  ConnectionPort(aConnection : Connection) : Word;

  /// Gets the Port of the destination for the connectiom
  ///
  /// @param name The name of the connection to get the port from
  ///
  /// @lib ConnectionPortNamed
  function  ConnectionPort(const name: String) : Word;

  /// Checks if any messages have been received for any open connections. 
  /// Messages received are added to the connection they were received from.
  ///
  /// @lib
  function HasMessages () : Boolean;

  /// Returns true if a server has messages that you can read.
  /// Use this to control a loop that reads all of the messages from
  /// a server.
  ///
  /// @lib HasMessageOnServer
  function HasMessages(svr: ServerSocket) : Boolean;

  /// Returns true if a connection has messages that you can read.
  /// Use this to control a loop that reads all of the messages from
  /// a connection.
  ///
  /// @lib HasMessageOnConnection
  function HasMessages(con: Connection) : Boolean;

  /// Returns true if a connection (found via its name) has messages that you can read.
  /// Use this to control a loop that reads all of the messages from
  /// a connection.
  ///
  /// @lib HasMessageOnConnectionNamed
  function HasMessages(const name: String) : Boolean; 

  /// Reads the next message that was sent to the connection. You use this
  /// to read the values that were sent to this connection.
  ///
  /// @param aConnection The connection to read the message from
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method ReadMessage
  function ReadMessage(aConnection : Connection): Message;
  
  /// Reads the next message that was sent to the connection or server (found from its name).
  /// You use this to read the values that were sent to this connection or server.
  ///
  /// @param name The name of the connection or server to read the message from
  ///
  /// @lib ReadMessageNamed
  function ReadMessage(const name: String): Message;

  /// Reads the next message from any of the clients that have connected to the server.
  ///
  /// @param svr The server to read the message from.
  ///
  /// @lib ReadMessageFromServer
  function ReadMessage(svr: ServerSocket): Message;

  /// Reads the data of the next message that was sent to the connection. You use this
  /// to read the values that were sent to this connection.
  ///
  /// @param aConnection The connection to read the message from
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method ReadMessageData
  function ReadMessageData(aConnection : Connection): String;
  
  /// Reads the data of the next message that was sent to the connection or server (found from its name).
  /// You use this to read the values that were sent to this connection or server.
  ///
  /// @param name The name of the connection or server to read the message from
  ///
  /// @lib ReadMessageDataNamed
  function ReadMessageData(const name: String): String;

  /// Reads the data of the next message from any of the clients that have connected to the server.
  ///
  /// @param svr The server to read the message from.
  ///
  /// @lib ReadMessageDataFromServer
  function ReadMessageData(svr: ServerSocket): String;


  /// Clears all of the messages from a connection.
  ///
  /// @lib ConnectionClearMessages
  ///
  /// @class Connection
  /// @method ClearMessages
  procedure ClearMessages(aConnection: Connection);

  /// Clears all of the messages from a server.
  ///
  /// @lib ServerClearMessages
  procedure ClearMessages(svr: ServerSocket);

  /// Clears the Messages from a connection or server.
  ///
  /// @lib ClearMessagesNamed
  procedure ClearMessages(const name: String);

  /// Gets the number of messages waiting to be read from this connection
  ///
  /// @lib ConnectionMessageCount
  ///
  /// @class Connection
  /// @method MessageCount
  function  MessageCount(aConnection : Connection) : LongInt;

  /// Gets the number of messages waiting to be read from the connection (found via its named)
  ///
  /// @lib MessageCountOnConnectionNamed
  function  MessageCount(const name: String) : LongInt;

  /// Gets the number of messages waiting to be read from this connection
  ///
  /// @lib ServerMessageCount
  function  MessageCount(svr: ServerSocket) : LongInt;

  /// Gets the data from a Message. This will be a string.
  ///
  /// @lib
  ///
  /// @class Message
  /// @getter Data
  function MessageData(msg: Message): String;

  /// Gets the protocol that was used to send the Message.
  ///
  /// @lib
  ///
  /// @class Message
  /// @getter Protocol
  function MessageProtocol(msg: Message): ConnectionType;

  /// Gets the connection used to send the message (TCP only).
  ///
  /// @lib
  ///
  /// @class Message
  /// @getter Connection
  function MessageConnection(msg: Message): Connection;

  /// Gets the host that sent the message.
  ///
  /// @lib
  ///
  /// @class Message
  /// @getter Host
  function MessageHost(msg: Message): String;

  /// Gets the port that the host sent the message from.
  ///
  /// @lib
  ///
  /// @class Message
  /// @getter Port
  function MessagePort(msg: Message): Word;


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
  function DecToHex(aDec : LongWord) : String;

  /// Converts a Hex String to a Decimal Value as a String.
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexToDecString 
  /// @sn hexToDecString:%s
  function HexToDecString(const aHex : String) : String;

  /// Converts a Hex String to an IPV4 Address (0.0.0.0)
  ///
  /// @param aHex The Hex String
  ///
  /// @lib
  /// @uname HexStrToIPv4 
  /// @sn hexStrToIPv4:%s
  function HexStrToIPv4(const aHex : String) : String;

  /// Converts an IP to a decimal value
  ///
  /// @param aIP The IP
  ///
  /// @lib
  /// @sn iPv4ToDec:%s
  function IPv4ToDec(const aIP : String) : LongWord;

  /// Converts an integer representation of a ip address to a string representation.
  ///
  /// @lib
  function IPv4ToStr(ip : LongWord) : String; 

//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------

  /// Closes the specified server socket. This will close all connections to
  /// the server, as will stop listening for new connections.
  ///
  /// @lib
  function CloseServer ( var svr: ServerSocket ) : Boolean;

  /// Closes the specified server socket. This will close all connections to
  /// the server, as will stop listening for new connections.
  ///
  /// @lib CloseServerNamed
  function CloseServer ( const name: String ) : Boolean;

  /// Frees the server.
  ///
  /// @lib
  ///
  /// @class ServerSocket
  /// @dispose
  ///
  /// @deprecated
  procedure FreeServer(var svr: ServerSocket);

  /// Closes the specified connection.
  ///
  /// @param aConnection  The Connection to close
  ///
  /// @lib
  ///
  /// @class Connection
  /// @method Close
  function CloseConnection (var aConnection : Connection) : Boolean;

  /// Closes the connection and frees resources.
  ///
  /// @deprecated
  ///
  /// @class Connection
  /// @dispose
  ///
  /// @lib
  procedure FreeConnection(var aConnection : Connection);

  /// Closes the specified connection.
  ///
  /// @param name  The name of the Connection to close
  ///
  /// @lib CloseConnectionNamed
  function CloseConnection (const name: String) : Boolean;
  
  /// Closes the specified Socket, removed it from the Socket Array, and removes
  /// the identifier from the NamedIndexCollection.
  /// Refers to UDP Listener Sockets
  ///
  /// @param aPort The identifier of the Host Socket.
  ///
  /// @lib
  function CloseUDPSocket      ( aPort : Word) : Boolean;

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
  procedure CloseAllServers ();

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

  /// Indicates the maximum size of a UDP message.
  ///
  /// @lib
  function UDPPacketSize(): Longint;

  /// Allows you to change the maximum size for a UDP message (sending and receiving)
  ///
  /// @lib
  procedure SetUDPPacketSize(val: Longint);
          
//=============================================================================
implementation
  uses SysUtils, sgUtils, sgNamedIndexCollection, 
    {$ifdef WINDOWS}
      Winsock2,
    {$endif}
    {$ifdef UNIX}
      BaseUnix, NetDB,
    {$endif}
    Sockets, sgShared, StrUtils, sgDriverSDL2Types, sgBackendTypes;
//=============================================================================

type
  ServerArray = array of ServerSocketPtr;
  ConnectionArray = array of ConnectionPtr;
  MessageArray = array of MessageData;
  PacketData = array [0..511] of Char;
  BytePtr = ^Byte;
  Bytes = array[0..3] of Byte;

var
  _Servers                : ServerArray;
  _Connections            : ConnectionArray;

  _ServerIds      : NamedIndexCollection;
  _ConnectionIds  : NamedIndexCollection;

  _UDPPacketSize : Integer = 1024;  


//----------------------------------------------------------------------------
// Internal Functions
//----------------------------------------------------------------------------

  procedure EnqueueTCPMessage( const aMsg : String; aConnection : ConnectionPtr); forward;
  procedure EnqueueUDPMessage( var messages: MessageArray; const msg : array of char; size: Integer; hostNum: UInt; portNum: Word); forward;
  procedure FreeConnection(var aConnection : ConnectionPtr); forward;
  procedure ShutConnection(con: ConnectionPtr); forward;
  function CloseConnection(aConnection : ConnectionPtr) : Boolean; forward;
  function MessageCount(aConnection : ConnectionPtr) : LongInt; forward;
  function MessageCount(svr: ServerSocketPtr) : LongInt; forward;

  function CreateConnection(const name: String; protocol: ConnectionType) : ConnectionPtr;
  begin
    New(result);
    result^.id          := CONNECTION_PTR;
    result^.name        := name;
    result^.ip          := 0;
    result^.stringIP    := '';
    result^.port        := 0;
    result^.protocol    := protocol;
    result^.partMsgData := '';
    result^.msgLen      := -1;
    result^.open        := true;
    result^.socket._socket     := nil;
    result^.socket.kind        := SGCK_UNKNOWN;
    SetLength(result^.messages, 0);
  end;

  function GetConnectionWithID(const aIP : LongWord; const aPort : Word; aprotocol : ConnectionType) : Connection;
  var
    i : LongInt;
  begin
    result := nil;
    for i := Low(_Connections) to High(_Connections) do
    begin
      if (_Connections[i]^.ip = aIP) and (_Connections[i]^.port = aPort) and (_Connections[i]^.protocol = aprotocol) then
      begin
        result := Connection(_Connections[i]);
      end;
    end;
  end;

  ///
  /// Reads a message from the connection and stores it in the connection's message queue
  ///
  /// return true if expecting more data...
  function ExtractData(var buffer: PacketData; aReceivedCount: LongInt; const aConnection : ConnectionPtr): Boolean;
  var
    msgLen    : LongInt;
    bufIdx    : LongInt = 0;               // Index of current data in the buffer
    i, missing, got: Integer;
    msg       : String;
    size      : Bytes;
  begin
    result := false;
    while bufIdx < aReceivedCount do      // Loop until all characters are extracted from the buffer
    begin  
      // Check if we already know the length of the message          
      if aConnection^.msgLen > 0 then
      begin
        msg := aConnection^.partMsgData;  // get old part of message
        msgLen := aConnection^.msgLen - Length(msg);  // Adjusted length for previous read
        aConnection^.msgLen := -1;  // reset this...
        aConnection^.partMsgData := ''; // clear
      end
      else
      begin
        msg := '';  // a new message

        if Length(buffer) - bufIdx < 4 then
        begin
          // WriteLn('length: ', Length(buffer));
          // WriteLn('Idx: ', bufIdx);
          missing := 4 - (Length(buffer) - bufIdx);
          // WriteLn('Missing ', missing);

          // need to read in size...
          for i := 0 to missing do
            buffer[Length(buffer) - 4 + i] := buffer[bufIdx + i]; // copy back to last 4 positions...
          
          // fill at the 4 positions for the size
          // WriteLn('Reading ', missing);

          got := _sg_functions^.network.read_bytes(@aConnection^.socket, @buffer[Length(buffer) - missing], missing);
          // WriteLn('got ', got);
          if got <> missing then
          begin
            RaiseWarning('Issue reading message size from network. Report to acain@swin.edu.au');
            exit;
          end;

          bufIdx := Length(buffer) - 4;
        end;

        size[0] := Byte(buffer[bufIdx]);
        size[1] := Byte(buffer[bufIdx + 1]);
        size[2] := Byte(buffer[bufIdx + 2]);
        size[3] := Byte(buffer[bufIdx + 3]);

        // WriteLn('bytes = ', size[0], ' ', size[1], ' ', size[2], ' ', size[3]);
        // WriteLn('calc = ', (size[0] shl 24) + (size[1] shl 16) + (size[2] shl 8) + size[3]);

        // data is Big Endian
        msgLen := (size[0] shl 24) + (size[1] shl 16) + (size[2] shl 8) + size[3];    // Get the length of the next message
        // WriteLn('msglen: ', msgLen);
        bufIdx += 4;
      end;
      
      for bufIdx := bufIdx to bufIdx + msgLen - 1 do
      begin
        if (bufIdx >= aReceivedCount) or (bufIdx > Length(buffer)) then 
        begin
          aConnection^.partMsgData := msg;
          aConnection^.msgLen      := msgLen;
          // WriteLn('Message: ', msg, ' ');
          // WriteLn('Part message: ', msg);
          result := true;
          exit;                           // Exit... end of buffer, but not end of message
        end;
        
        msg += buffer[bufIdx];
      end;
      
      EnqueueTCPMessage(msg, aConnection);
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

  function MyIP() : String;
  begin
    result := '127.0.0.1';
  end;
  
  
//----------------------------------------------------------------------------
// Connection Handling
//----------------------------------------------------------------------------

  function EstablishConnection(con: ConnectionPtr; const host: String; port: Word; protocol: ConnectionType): Boolean;
  var
    socket : psg_network_connection;
  begin
    socket := @con^.socket;

    con^.stringIP      := host;
    con^.port          := port;
    con^.protocol      := protocol;

    
    if protocol = TCP then
    begin
      socket^ := _sg_functions^.network.open_tcp_connection(PChar(host), port);

      if Assigned(socket^._socket) and (socket^.kind = SGCK_TCP) then
      begin
        con^.IP := _sg_functions^.network.network_address(socket);
        result  := true;
      end
      else
      begin
        result := false;
      end;
    end
    else //UDP
    begin
      socket^ := _sg_functions^.network.open_udp_connection(0);
      // con^.IP := IPv4ToDec(HostIP(host));
      con^.IP := _sg_functions^.network.network_address(socket);
      result  := true;
    end

    // WriteLn('client con = ', HexStr(con^._socket), ' ', con^.kind);
  end;  

  function NameForConnection(const host: String; port: Word) : String;
  begin
    result := host + ':' + IntToStr(port);
  end;

  function OpenConnection(const name, host: String; port: Word; protocol: ConnectionType) : Connection;
  var
    idx: Integer;
    conPtr: ConnectionPtr;
  begin
    conPtr := CreateConnection(name, protocol);

    if EstablishConnection(conPtr, host, port, protocol) then
    begin
      SetLength(_Connections, Length(_Connections) + 1);
      idx := AddName(_ConnectionIds, name);
      // WriteLn('Adding at idx: ', idx);

      if idx <> High(_Connections) then
      begin
        RaiseWarning('ERROR adding connection -- named index collection out of sync. Contact SwinGame dev team.');
      end;
      // WriteLn('Connection is ', HexStr(conPtr));
      _Connections[idx] := conPtr;
    end
    else
    begin
      Dispose(conPtr);
      conPtr := nil;
    end;

    result := Connection(conPtr);
  end;

  function OpenConnection(const name, host: String; port: Word) : Connection;
  begin
    result := OpenConnection(name, host, port, TCP);
  end;

  function OpenConnection(const host: String; port: Word) : Connection;
  begin
    result := OpenConnection(NameForConnection(host, port), host, port);
  end;

  function ConnectionNamed(const name: String): Connection;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := Connection(_Connections[idx])
    else
      result := nil;
  end;

  procedure Reconnect(aConnection : Connection);
  var
    host : String;
    port : Word;
    conPtr: ConnectionPtr;
  begin
    conPtr := ToConnectionPtr(aConnection);
    if not Assigned(conPtr) then exit;

    host := conPtr^.stringIP;
    port := conPtr^.port;
    
    // close old socket
    _sg_functions^.network.close_connection(@conPtr^.socket);

    conPtr^.open := EstablishConnection(conPtr, host, port, conPtr^.protocol);
  end;

  procedure Reconnect(const name: String);
  begin
    Reconnect(ConnectionNamed(name));  
  end;

  function CreateServer(const name: String; port: Word; protocol: ConnectionType) : ServerSocket;
  var
    con : sg_network_connection;
    idx: Integer;
    svrPtr: ServerSocketPtr;
  begin
    result := nil;

    if protocol = UDP then
      con := _sg_functions^.network.open_udp_connection(port)
    else
      con := _sg_functions^.network.open_tcp_connection(nil, port);

    // WriteLn('svr con = ', HexStr(con^._socket), ' ', con^.kind);

    if Assigned(con._socket) and ((con.kind = SGCK_TCP) or (con.kind = SGCK_UDP)) then
    begin
      New(svrPtr);
      svrPtr^.id := SERVER_SOCKET_PTR;
      svrPtr^.name := name;
      svrPtr^.socket := con;
      svrPtr^.port := port;
      svrPtr^.newConnections := 0;
      svrPtr^.protocol := protocol;
      SetLength(svrPtr^.connections, 0);

      SetLength(_Servers, Length(_Servers) + 1);

      idx := AddName(_ServerIds, name);
      if idx <> High(_Servers) then
      begin
        RaiseWarning('ERROR adding server -- named index collection out of sync. Contact SwinGame dev team.');
      end;
      _Servers[High(_Servers)] := svrPtr;
    end;

    result := ServerSocket(svrPtr);
  end;

  function CreateServer(const name: String; port : Word) : ServerSocket;
  begin
    result := CreateServer(name, port, TCP);
  end;

  function ServerNamed(const name: String): ServerSocket;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ServerIds, name);
    if idx >= 0 then
      result := ServerSocket(_Servers[idx])
    else
      result := nil;
  end;

  function ConnectionOpen(con: Connection) : Boolean;
  var
    conPtr: ConnectionPtr;
  begin
    conPtr := ToConnectionPtr(con);
    if Assigned(conPtr) and conPtr^.open then 
      result := true
    else
      result := false;
  end;

  function ConnectionOpen(const name: String) : Boolean;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := ConnectionOpen(Connection(_Connections[idx]))
    else
      result := false;
  end;

  function AcceptNewConnection(server: ServerSocket) : Boolean;
  var
    con : sg_network_connection;
    client: ConnectionPtr;
    ip, port: Integer;
    svrPtr: ServerSocketPtr;
  //   lTempSocket : PTCPSocket = nil;
  //   lNewConnection : Connection;
  //   i : LongInt;
  begin  
    result := false;
    svrPtr := ToServerSocketPtr(server);
    if not Assigned(svrPtr) then exit;

    svrPtr^.newConnections := 0;
    // WriteLn('Network port Pascal 0: ', HexStr(_sg_functions^.network.network_port));
    con := _sg_functions^.network.accept_new_connection(@svrPtr^.socket);

    // WriteLn('svr client con = ', HexStr(con._socket), ' ', con.kind);

    if Assigned(con._socket) and (con.kind = SGCK_TCP) then
    begin
      // WriteLn('Network port Pascal 1: ', HexStr(_sg_functions^.network.network_port));
      ip := _sg_functions^.network.network_address(@con);

      // WriteLn('Network port Pascal 2: ', HexStr(_sg_functions^.network.network_port));

      port := _sg_functions^.network.network_port(@con);

      client := CreateConnection(svrPtr^.name + '->' + NameForConnection(IPv4ToStr(ip), port), TCP);
      client^.IP := ip;
      client^.port := port;

      // WriteLn(client^.name, ' ', client^.port);

      client^.socket := con;

      SetLength(svrPtr^.connections, Length(svrPtr^.connections) + 1);
      svrPtr^.connections[High(svrPtr^.connections)] := client;
      svrPtr^.newConnections := 1;
      result := true;
      // WriteLn('Added ', HexStr(client), ' to ', svrPtr^.name);
    end;
  end;

  function AcceptAllNewConnections () : Boolean;
  var
    i: Integer;
  begin
    result := false;
    for i := 0 to High(_Servers) do
    begin
      if AcceptNewConnection( ServerSocket(_Servers[i]) ) then result := true;
    end;
  end;

  function ServerHasNewConnection(server: ServerSocket) : Boolean;
  var
    svrPtr: ServerSocketPtr;
  begin
    svrPtr := ToServerSocketPtr(server);
    if Assigned(svrPtr) then 
      result := svrPtr^.newConnections > 0
    else
      result := false;
  end;

  function ServerHasNewConnection(const name: String) : Boolean;
  begin
    result := ServerHasNewConnection(ServerNamed(name));
  end;

  function HasNewConnections() : Boolean;
  var
    svr: Integer;
  begin
    for svr := 0 to High(_Servers) do
    begin
      if ServerHasNewConnection( ServerSocket(_Servers[svr])) then
      begin
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

//----------------------------------------------------------------------------
// Message Handling
//----------------------------------------------------------------------------

  function UDPPacketSize(): Longint;
  begin
    result := _UDPPacketSize;
  end;

  procedure SetUDPPacketSize(val: Longint);
  begin
    _UDPPacketSize := val;
  end;

  function ReadUDPMessageFrom(socket: psg_network_connection; var messages: MessageArray): Boolean;
  var
    size, host: UInt;
    port: Word;
    data: array of char;
    times: Integer;
  begin
    result := false;
    if not Assigned(socket) then exit;

    if _sg_functions^.network.connection_has_data(socket) > 0 then
    begin
      result := true;
      SetLength(data, _UDPPacketSize);
      // WriteLn('getting data');

      times := 0;

      repeat
        size := Length(data);
        host := 0;
        port := 0;
        // WriteLn('reading data...');
        _sg_functions^.network.read_udp_message(socket, @host, @port, @data[0], @size);
        
        // WriteLn('Got message ', size, ' ', IPv4ToStr(host), ' ', port);

        if (size > 0) or (host > 0) then
        begin
          EnqueueUDPMessage(messages, data, size, host, port);
        end;  

        times += 1;
      until ((size = 0) and (host = 0)) or (times >= 10);
    end;
  end;

  function CheckConnectionForData(con: ConnectionPtr): Boolean;
  var
    received: Integer;
    buffer: PacketData;
    gotData: Boolean = false;
    times: Integer = 0;
  begin
    result := false;
    if (not Assigned(con)) or (not Assigned(con^.socket._socket)) then exit;
    if con^.open = false then exit;

    // WriteLn(HexStr(con), ' -> ', HexStr(con^.socket) );
    if _sg_functions^.network.connection_has_data(@con^.socket) > 0 then
    begin
      result := true;
      // WriteLn('getting data');
      repeat
        // WriteLn('checking...');

        if con^.protocol = TCP then
        begin
          received := _sg_functions^.network.read_bytes(@con^.socket, @buffer[0], 512);

          if received <= 0 then
          begin
            ShutConnection(con);
            exit;
          end;

          gotData := ExtractData(buffer, received, con);
        end
        else //UDP
        begin
          gotData := ReadUDPMessageFrom(@con^.socket, con^.messages);
        end;

        times += 1;
      until (not gotData) or (times >= 10);
    end;
  end;

  function CheckUDPSocketForData(svr: ServerSocketPtr): Boolean;
  begin
    if Assigned(svr) then
      result := ReadUDPMessageFrom(@svr^.socket, svr^.messages)
    else
      result := false;
  end;

  procedure CheckNetworkActivity();
  var
    svr, i: Integer;
    gotData: Boolean;
  begin
    AcceptAllNewConnections();
    gotData := true;

    // check if there is data on the network
    while (_sg_functions^.network.network_has_data() > 0) and (gotData) do
    begin
      gotData := false;

      // WriteLn('should be some data...');
      for svr := 0 to High(_servers) do
      begin
        if _servers[svr]^.protocol = TCP then
        begin
          for i := 0 to High(_servers[svr]^.connections) do
          begin
            // WriteLn('Checking svr ', svr, ' connection ', i, ' ', HexStr(_servers[svr]^.connections[i]));
            gotData := CheckConnectionForData(_servers[svr]^.connections[i]) or gotData;
          end;
        end
        else //UDP
        begin
          gotData := CheckUDPSocketForData(_servers[svr]) or gotData;
        end;
      end;

      for i := 0 to High(_Connections) do
      begin
        // WriteLn('Checking connection ', i, ' ', HexStr(_Connections[i]));
        gotData := CheckConnectionForData(_Connections[i]) or gotData;
      end;
    end;

    // if (_sg_functions^.network.network_has_data() > 0) and (not gotData) then
    // begin
    //   WriteLn('hmmmm -- no data but data expected!');
    // end;
  end;

  function HasMessages() : Boolean;
  var
    svr, i: Integer;
  begin
    // WriteLn('should be some data...');
    for svr := 0 to High(_servers) do
    begin
      if HasMessages( ServerSocket(_servers[svr])) then
      begin
        result := true;
        exit;
      end;
    end;

    for i := 0 to High(_Connections) do
    begin
      if MessageCount(Connection(_Connections[i])) > 0 then
      begin
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  procedure CloseMessage(msg: MessagePtr);
  begin
    if Assigned(msg) then
    begin
      msg^.id := NONE_PTR;
      msg^.data := '';
      msg^.connection := nil;
      msg^.host := '';
      msg^.port := 0;

      Dispose(msg);
    end;
  end;

  procedure CloseMessage(msg: Message);
  begin
    CloseMessage(ToMessagePtr(msg));
  end;

  function SendMessageTo(const aMsg : String; conPtr: ConnectionPtr) : Boolean;
  var
    len, i : LongInt;
    buffer: array of Byte;
    size: Bytes;
  begin
    result := false;

    if not Assigned(conPtr) or conPtr^.open = false then exit;

    if conPtr^.protocol = TCP then
    begin
      // if Length(aMsg) > 255 then begin RaiseWarning('SendMessageTo: SwinGame messages must be less than 256 characters in length'); exit; end;
      size := Bytes(NtoBE(LongInt(Length(aMsg))));
      // WriteLn('send size: ', size[0], ' ', size[1], ' ', size[2], ' ', size[3], ' ');

      len := Length((aMsg)) + 4;
      SetLength(buffer, len);

      for i := 0 to len - 1 do
      begin
        if i < 4 then
        begin
          buffer[i] := size[i];
          // WriteLn('* ', buffer[i]);
        end
        else
        begin
          buffer[i] := Byte(aMsg[i - 3]); // 1 to Length
        end;
      end;

      // WriteLn('sending');
      if _sg_functions^.network.send_bytes(@conPtr^.socket, @buffer[0], len) = len then
      begin
        // WriteLn('sent');
        result := true;
      end
      else
      begin
        // Error on read... close connection
        ShutConnection(conPtr);
      end;
    end
    else //UDP
    begin
      // WriteLn('Sending udp packet');
      if Length(aMsg) < 1024 then
      begin
        _sg_functions^.network.send_udp_message(@conPtr^.socket, PChar(conPtr^.stringIP), conPtr^.port, @aMsg[1], Length(aMsg));
        result := true;
      end
      else
      begin
        result := false; // message too long
      end;
    end;

    // WriteLn('bye');
  end;

  function SendMessageTo(const aMsg : String; aConnection: Connection) : Boolean;
  begin
    result := SendMessageTo(aMsg, ToConnectionPtr(aConnection));
  end;

  function SendMessageTo(const aMsg, name: String): Boolean;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := SendMessageTo(aMsg, Connection(_Connections[idx]))
    else
      result := false;
  end;

  procedure BroadcastMessage(const aMsg : String; svrPtr: ServerSocketPtr);
  var
    i: Integer;
  begin
    if not Assigned(svrPtr) then exit;

    for i := 0 to High(svrPtr^.connections) do
    begin
      SendMessageTo(aMsg, svrPtr^.connections[i]);
    end;
  end;

  procedure BroadcastMessage(const aMsg : String; svr: ServerSocket);
  begin
    BroadcastMessage(aMsg, ToServerSocketPtr(svr));
  end;

  procedure BroadcastMessage(const aMsg : String; const name: String);
  begin
    BroadcastMessage(aMsg, ServerNamed(name));
  end;

  procedure BroadcastMessage(const aMsg : String);
  var
    i: LongInt;
  begin
    for i := 0 to High(_Servers) do
    begin
      BroadcastMessage(aMsg, _Servers[i]);
    end;

    for i := 0 to High(_Connections) do
    begin
      SendMessageTo(aMsg, _Connections[i]);
    end;
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

  function CreateUDPHostProcedure(const aPort : Word) : LongInt;
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
   
  function CreateUDPConnectionProcedure(const aDestIP : String; const aDestPort, aInPort : Word) : Connection; 
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

    //     EnqueueTCPMessage(lMsg, lConnection);
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
  
  procedure BroadcastUDPMessage(const aMsg : String);
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
  
  function CloseServer ( svr: ServerSocketPtr ) : Boolean;
  var
    i, idx: LongInt;
    toClose: ServerSocketPtr;
  begin
    result := False;
    if not Assigned(svr) then exit;

    // copy pointer in case parameter is also array element
    toClose := svr;

    // close all connections to the server
    while Length(toClose^.connections) > 0 do
    begin
      CloseConnection(toClose^.connections[High(toClose^.connections)]);
    end;

    // close the socket
    _sg_functions^.network.close_connection(@toClose^.socket);

    idx := IndexOf(_ServerIds, toClose^.name);
    if _Servers[idx] <> toClose then RaiseWarning('Error closing server, names out of sync. Contact SwinGame dev team.');
    RemoveName(_ServerIds, idx);

    // remove from list of servers
    for i := idx + 1 to High(_Servers) do
    begin
      _Servers[i-1] := _Servers[i];
    end;
    // nil at this point -- in case it is in the array... cannot be earlier as it may be needed
    svr := nil;
    SetLength(_Servers, Length(_Servers) - 1);

    // free memory
    Dispose(toClose);
    result := True;
  end;

  function CloseServer ( var svr: ServerSocket ) : Boolean;
  begin
    result := CloseServer(ToServerSocketPtr(svr));
    svr := nil;
  end;

  procedure FreeServer( var svr: ServerSocket );
  begin
    CloseServer(svr);
  end;


  function CloseServer ( const name: String ) : Boolean;
  var
    svr: ServerSocket;
  begin
    svr := ServerNamed(name);
    result := CloseServer(svr);
  end;

  function CloseUDPSocketProcedure(const aPort : Word) : Boolean;
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

  procedure CloseAllServers();
  begin    
    while Length(_Servers) > 0 do
    begin
      CloseServer(_Servers[High(_Servers)]);
    end;
  end;

  procedure CloseAllConnections();
  begin
    while Length(_Connections) > 0 do
      CloseConnection(_Connections[High(_Connections)]);
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
      
  function HexToDecString(const aHex : String) : String;
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
  
  function HexStrToIPv4(const aHex : String) : String;
  begin
    result :=       HexToDecString(aHex[1] + aHex[2]);
    result += '.' + HexToDecString(aHex[3] + aHex[4]);
    result += '.' + HexToDecString(aHex[5] + aHex[6]);
    result += '.' + HexToDecString(aHex[7] + aHex[8]);
  end;

  function IPv4ToDec(const aIP : String) : LongWord;
  var
    w, x, y, z : LongInt;
  begin
    result := 0;
    try
      w := StrToInt(ExtractDelimited(1, aIP, ['.']));
      x := StrToInt(ExtractDelimited(2, aIP, ['.']));
      y := StrToInt(ExtractDelimited(3, aIP, ['.']));
      z := StrToInt(ExtractDelimited(4, aIP, ['.']));
      result := 16777216 * w + 65536 * x + 256 * y + z;
      // WriteLn('Result: ', result);      
    except
    end;
  end;

  function IPv4ToStr(ip : LongWord) : String;
  var
    parts: Bytes;
  begin
    parts := Bytes(ip);

    result := IntToStr(parts[3]) + '.' + IntToStr(parts[2]) + '.' + IntToStr(parts[1]) + '.' + IntToStr(parts[0]);
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
// Connection
//----------------------------------------------------------------------------

  function ConnectionCount(svrPtr: ServerSocketPtr) : LongInt;
  begin
    result := 0;
    if (not Assigned(svrPtr)) then exit;
    result := Length(svrPtr^.connections);
  end;

  function ConnectionCount(server: ServerSocket) : LongInt;
  begin
    result := ConnectionCount(ToServerSocketPtr(server));
  end;

  function ConnectionCount(const name: String) : LongInt;
  begin
    result := ConnectionCount(ServerNamed(name));
  end;

  function RetreiveConnection(svrPtr: ServerSocketPtr; idx: LongInt) : ConnectionPtr;
  begin
    result := nil;
    if (not Assigned(svrPtr)) or (idx < 0) or (idx > High(svrPtr^.connections)) then exit;
    result := svrPtr^.connections[idx];
  end;

  function RetreiveConnection(server: ServerSocket; idx: LongInt) : Connection;
  begin
    result := Connection( RetreiveConnection(ToServerSocketPtr(server), idx) );
  end;

  function RetreiveConnection(const name: String; idx: LongInt) : Connection;
  begin
    result := RetreiveConnection(ServerNamed(name), idx);
  end;

  function LastConnection(server: ServerSocket) : Connection;
  var
    svrPtr: ServerSocketPtr;
  begin
    svrPtr := ToServerSocketPtr(server);

    result := nil;
    if (not Assigned(svrPtr)) or (Length(svrPtr^.connections) <= 0) then exit;
    result := Connection(svrPtr^.connections[High(svrPtr^.connections)]);
  end;

  function LastConnection(const name: String) : Connection;
  begin
    result := LastConnection(ServerNamed(name));
  end;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------
  
  procedure EnqueueTCPMessage( const aMsg : String; aConnection : ConnectionPtr);
  begin
    if not Assigned(aConnection) then exit;

    // WriteLn('Adding message: ', aMsg);
    SetLength(aConnection^.messages, Length(aConnection^.messages) + 1);

    with aConnection^.messages[High(aConnection^.messages)] do
    begin
      id := MESSAGE_PTR;
      data := aMsg;
      protocol := TCP;
      connection := aConnection;
      host := aConnection^.stringIP;
      port := aConnection^.port;
    end;
  end;

  procedure EnqueueUDPMessage(var messages: MessageArray; const msg : array of char; size: Integer; hostNum: UInt; portNum: Word);
  var
    i: Integer;
  begin
    // WriteLn('Adding message: ', aMsg);
    SetLength(messages, Length(messages) + 1);

    with messages[High(messages)] do
    begin
      id := MESSAGE_PTR;
      data := '';
      for i := 0 to size - 1 do
      begin
        data += msg[i];
      end;

      protocol := UDP;
      connection := nil;
      host := IPv4ToStr(hostNum);
      port := portNum;
    end;
  end;

  function HasMessages(con: ConnectionPtr) : Boolean; 
  begin
    if not Assigned(con) then result := false
    else result := Length(con^.messages) > 0;
  end;

  function HasMessages(con: Connection) : Boolean; 
  begin
    result := HasMessages(ToConnectionPtr(con));
  end;

  function HasMessages(const name: String) : Boolean;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := HasMessages(_Connections[idx])
    else
    begin
      result := HasMessages(ServerNamed(name));
    end;
  end;

  function HasMessages(svr: ServerSocketPtr) : Boolean;
  var
    i: Integer;
  begin
    result := true;

    if Assigned(svr) then
    begin

      if Length(svr^.messages) > 0 then
      begin
        exit;    
      end;

      for i := 0 to High(svr^.connections) do
      begin
        if MessageCount(svr^.connections[i]) > 0 then
        begin
          exit;
        end;
      end;
    end;

    result := false;
  end;

  function HasMessages(svr: ServerSocket) : Boolean;
  begin
    result := HasMessages(ToServerSocketPtr(svr));
  end;

  function PopMessage(var messages: MessageArray): MessagePtr;
  var
    i: Integer;
  begin      
    // Get the data from the first message
    New(result);
    result^ := messages[0];

    for i := 1 to High(messages) do
    begin
      messages[i - 1] := messages[i];
    end;

    SetLength(messages, Length(messages) - 1);
  end;

  function ReadMessage(aConnection: ConnectionPtr): MessagePtr;
  begin      
    if not HasMessages(aConnection) then
      result := nil
    else
      // Get the data from the first message
      result := PopMessage(aConnection^.messages);
  end;

  function ReadMessage(aConnection: Connection): Message;
  begin
    result := Message(ReadMessage(ToConnectionPtr(aConnection)));
  end;
  
  function ReadMessage(const name: String): Message;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := Message(ReadMessage(_Connections[idx]))
    else
      result := ReadMessage(ServerNamed(name));
  end;

  function ReadMessage(svr: ServerSocketPtr): MessagePtr;
  var
    i: Integer;
    con: ConnectionPtr;
  begin
    result := nil;

    for i := 0 to ConnectionCount(svr) - 1 do
    begin
      con := RetreiveConnection(svr, i);
      if HasMessages(con) then
      begin
        result := ReadMessage(con);
        exit;
      end;
    end;

    if Length(svr^.messages) > 0 then
    begin
      result := PopMessage(svr^.messages);
      exit;
    end;
  end;

  function ReadMessage(svr: ServerSocket): Message;
  begin
    result := Message(ReadMessage(ToServerSocketPtr(svr)));
  end;

  function ReadMessageData(aConnection : Connection): String;
  var
    msg: MessagePtr;
  begin
    msg := ReadMessage(ToConnectionPtr(aConnection));
    if Assigned(msg) then
    begin
      result := msg^.data;
      CloseMessage(msg);
    end
    else
      result := '';
  end;
  
  function ReadMessageData(const name: String): String;
  var
    msg: MessagePtr;
  begin
    msg := ToMessagePtr(ReadMessage(name));
    if Assigned(msg) then
    begin
      result := msg^.data;
      CloseMessage(msg);      
    end
    else
      result := '';
  end;

  function ReadMessageData(svr: ServerSocket): String;
  var
    msg: MessagePtr;
  begin
    msg := ReadMessage(ToServerSocketPtr(svr));
    if Assigned(msg) then
    begin
      result := msg^.data;
      CloseMessage(msg);
    end
    else
      result := '';
  end;

  
  procedure ClearMessages(aConnection : ConnectionPtr);
  begin
    if not Assigned(aConnection) then exit;
    SetLength(aConnection^.messages, 0);
  end;

  procedure ClearMessages(aConnection : Connection);
  begin
    ClearMessages(ToConnectionPtr(aConnection));
  end;

  procedure ClearMessages(svr: ServerSocketPtr);
  begin
    if not Assigned(svr) then exit;
    SetLength(svr^.messages, 0);
  end;

  procedure ClearMessages(svr: ServerSocket);
  begin
    ClearMessages(ToServerSocketPtr(svr));
  end;

  procedure ClearMessages(const name: String);
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      ClearMessages(_Connections[idx])
    else
      ClearMessages(ServerNamed(name));
  end;

  function ConnectionIP(aConnection : ConnectionPtr) : LongWord;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.ip;
  end;

  function ConnectionIP(aConnection : Connection) : LongWord;
  begin
    result := ConnectionIP(ToConnectionPtr(aConnection));
  end;

  function ConnectionIP(const name: String) : LongWord;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := ConnectionIP(_Connections[idx])
    else
      result := 0;
  end;

  function ConnectionPort(aConnection : ConnectionPtr) : Word;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := aConnection^.port;
  end;

  function ConnectionPort(aConnection : Connection) : Word;
  begin
    result := ConnectionPort(ToConnectionPtr(aConnection));
  end;

  function ConnectionPort(const name: String) : Word;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := ConnectionPort(_Connections[idx])
    else
      result := 0;
  end;

  function MessageCount(aConnection : ConnectionPtr) : LongInt;
  begin
    result := 0;
    if not Assigned(aConnection) then exit;
    result := Length(aConnection^.messages);
  end;

  function MessageCount(aConnection : Connection) : LongInt;
  begin
    result := MessageCount(ToConnectionPtr(aConnection));
  end;

  function MessageCount(svr: ServerSocketPtr) : LongInt;
  begin
    result := 0;
    if not Assigned(svr) then exit;
    result := Length(svr^.messages);
  end;

  function MessageCount(svr: ServerSocket) : LongInt;
  begin
    result := MessageCount(ToServerSocketPtr(svr));
  end;

  function MessageCount(const name: String) : LongInt;
  var
    idx: Integer;
  begin
    idx := IndexOf(_ConnectionIds, name);
    if idx >= 0 then
      result := MessageCount(_Connections[idx])
    else
      result := MessageCount(ServerNamed(name));
  end;

//----------------------------------------------------------------------------
// Message methods
//----------------------------------------------------------------------------

  function MessageData(msg: MessagePtr): String;
  begin
    if Assigned(msg) then
      result := msg^.data
    else
      result := '';
  end;

  function MessageData(msg: Message): String;
  begin
    result := MessageData(ToMessagePtr(msg));
  end;

  function MessageProtocol(msg: MessagePtr): ConnectionType;
  begin
    if Assigned(msg) then
      result := msg^.protocol
    else
      result := UnknownConnection;
  end;

  function MessageProtocol(msg: Message): ConnectionType;
  begin
    result := MessageProtocol(ToMessagePtr(msg));
  end;

  function MessageConnection(msg: MessagePtr): ConnectionPtr;
  begin
    if Assigned(msg) then
      result := msg^.connection
    else
      result := nil;
  end;

  function MessageConnection(msg: Message): Connection;
  begin
    result := Connection( MessageConnection(ToMessagePtr(msg)) );
  end;

  function MessageHost(msg: MessagePtr): String;
  begin
    if Assigned(msg) then
      result := msg^.host
    else
      result := '';
  end;

  function MessageHost(msg: Message): String;
  begin
    result := MessageHost(ToMessagePtr(msg));
  end;

  function MessagePort(msg: MessagePtr): Word;
  begin
    if Assigned(msg) then
      result := msg^.port
    else
      result := 0;
  end;

  function MessagePort(msg: Message): Word;
  begin
    result := MessagePort(ToMessagePtr(msg));
  end;


//----------------------------------------------------------------------------
// Close
//----------------------------------------------------------------------------

  function IndexOfConnection(con: ConnectionPtr; const list: array of ConnectionPtr): Integer;
  begin
    for result := 0 to High(list) do
    begin
      // WriteLn('Checking ', HexStr(con), ' = ', HexStr(list[result]));
      if list[result] = con then exit;
    end;
    result := -1;
  end;

  procedure RemoveConnection(idx: Integer; var list: ConnectionArray);
  var
    i: Integer;
  begin
    // WriteLn(' -- Removing Connection ', idx, ' from ', Length(list));
    for i := idx to High(list) - 1 do
    begin
      list[i] := list[i + 1];
    end;
    SetLength(list, Length(list) - 1);
    // WriteLn('There are now ', Length(list), ' connections.');
  end;

  procedure ShutConnection(con: ConnectionPtr);
  begin
    if con^.open then
    begin
      con^.open := false;
      _sg_functions^.network.close_connection(@con^.socket);
    end;
  end;

  function CloseConnection(var aConnection : Connection) : Boolean;
  begin
    result := CloseConnection(ToConnectionPtr(aConnection));
  end;

  procedure FreeConnection(var aConnection : Connection);
  begin
    CloseConnection(aConnection);
  end;

  function CloseConnection(aConnection : ConnectionPtr) : Boolean;
  var
    idx, svr: Integer;
    toClose: ConnectionPtr;
  begin
    // WriteLn('Closing connection ', HexStr(aConnection));
    result := false;
    if not Assigned(aConnection) then begin WriteLn('Error freeing connection'); exit; end;

    // clear all of the messages
    ClearMessages(aConnection);

    // close socket
    ShutConnection(aConnection);

    toClose := aConnection; //so we keep track of the pointer, in case we are removing from array
    idx := IndexOf(_ConnectionIds, toClose^.name);

    if idx > -1 then
    begin
      // nil aConnection before removing from array in case we have the reference to the array
      // in which case it would corrupt the array's new value
      aConnection := nil;
      result := true;
      RemoveName(_ConnectionIds, idx);
      // WriteLn('Removing from clients...');
      RemoveConnection(idx, _Connections);
    end
    else // check if the connection is a client of a server
    begin
      // WriteLn('searching servers.');
      for svr := 0 to High(_Servers) do
      begin
        idx := IndexOfConnection(toClose, _Servers[svr]^.Connections);
        // WriteLn('Server:', svr, ' idx:', idx);
        if idx > -1 then
        begin
          result := true;
          // now nil the pointer...
          aConnection := nil;
          // WriteLn('Removing from server...');
          RemoveConnection(idx, _Servers[svr]^.Connections);
          break;
        end
      end;
    end;

    FreeConnection(toClose);
  end;

  function CloseConnection (const name: String) : Boolean;
  var
    con: Connection;
  begin
    con := ConnectionNamed(name);
    result := CloseConnection(con);
  end;

  function CloseUDPSocket( aPort : Word) : Boolean;
  begin
    result := false; //NetworkingDriver.CloseUDPSocket(aPort);    
  end;

  procedure CloseAllUDPSockets();
  begin        
    // NetworkingDriver.CloseAllUDPSocket();    
  end;

  procedure FreeConnection(var aConnection : ConnectionPtr);
  begin
    ClearMessages(aConnection);
    CallFreeNotifier(aConnection);
    Dispose(aConnection);
    aConnection := nil;
  end;

  procedure ReleaseAllConnections();
  begin
    CloseAllConnections();
    CloseAllServers();
  end;

//=============================================================================

  initialization 
  begin
    // Create the dictionaries used to manage the connection and server names
    // WriteLn('Network port Pascal b-0: ', HexStr(_sg_functions^.network.network_port));
    InitNamedIndexCollection(_ConnectionIds);
    InitNamedIndexCollection(_ServerIds);
    // WriteLn('Network port Pascal b-1: ', HexStr(_sg_functions^.network.network_port));
  end;

  finalization
  begin
    ReleaseAllConnections();

    FreeNamedIndexCollection(_ConnectionIds);
    FreeNamedIndexCollection(_ServerIds);
  end;
end.