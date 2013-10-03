unit sgDriverNetworking;

interface
uses sgShared, sgTypes, sgNamedIndexCollection;

type

    //TCP Connection
    CreateTCPHostProcedure             = function (const aPort : LongInt) : Boolean;
    CreateTCPConnectionProcedure   		 = function (const aDestIP : String; const aDestPort : LongInt) : Connection;
    AcceptTCPConnectionProcedure 			 = function () : LongInt;

    //TCP Message
    BroadcastTCPMessageProcedure       = procedure (const aMsg : String);
    SendTCPMessageProcedure            = function (const aMsg : String; const aConnection : Connection) : Connection;
    TCPMessageReceivedProcedure        = function () : Boolean;

    //UDP Connection
    CreateUDPHostProcedure             = function (const aPort : LongInt) : LongInt;
    CreateUDPConnectionProcedure       = function (const aDestIP : String; const aDestPort, aInPort : LongInt) : Connection; 
		
		//UDP Message
    UDPMessageReceivedProcedure        = function () : Boolean;
    SendUDPMessageProcedure            = function (const aMsg : String; const aConnection : Connection) : Boolean;
		BroadcastUDPMessageProcedure			 = procedure (const aMsg : String);

    //Close Sockets
    CloseTCPHostSocketProcedure        = function (const aPort: LongInt) : Boolean;  
    CloseConnectionProcedure           = function (var aConnection : Connection; const aCloseUDPSocket : Boolean) : Boolean;
    CloseUDPSocketProcedure      			 = function (const aPort : LongInt) : Boolean;

    CloseAllTCPHostSocketProcedure     = procedure();
    CloseAllConnectionsProcedure       = procedure(const aCloseUDPSockets : Boolean);
    CloseAllUDPSocketProcedure  			 = procedure();

    FreeAllNetworkingResourcesProcedure = procedure();

    MyIPProcedure                      = function () : String;
		ConnectionCountProcedure					 = function () : LongInt;
		RetreiveConnectionProcedure				 = function (const aConnectionAt : LongInt) : Connection;
		

  NetworkingDriverRecord = record
    CreateTCPHost               : CreateTCPHostProcedure;
    CreateTCPConnection     		: CreateTCPConnectionProcedure;
    AcceptTCPConnection   		  : AcceptTCPConnectionProcedure;
		
    TCPMessageReceived          : TCPMessageReceivedProcedure;
    BroadcastTCPMessage         : BroadcastTCPMessageProcedure;
    SendTCPMessage	            : SendTCPMessageProcedure;

    CreateUDPHost	              : CreateUDPHostProcedure;
    CreateUDPConnection         : CreateUDPConnectionProcedure;
		
    UDPMessageReceived          : UDPMessageReceivedProcedure;
    SendUDPMessage              : SendUDPMessageProcedure;
		BroadcastUDPMessage					: BroadcastUDPMessageProcedure;

    CloseTCPHostSocket          : CloseTCPHostSocketProcedure;  
    CloseConnection             : CloseConnectionProcedure;         
    CloseUDPSocket        			: CloseUDPSocketProcedure;     

    MyIP                        : MyIPProcedure; 
		ConnectionCount							: ConnectionCountProcedure;
		RetreiveConnection					: RetreiveConnectionProcedure;

    CloseAllTCPHostSocket       : CloseAllTCPHostSocketProcedure;     
    CloseAllConnections         : CloseAllConnectionsProcedure; 
    CloseAllUDPSocket     			: CloseAllUDPSocketProcedure; 

    FreeAllNetworkingResources  : FreeAllNetworkingResourcesProcedure;
  end;
      
var
  NetworkingDriver : NetworkingDriverRecord;

implementation
uses
  {$IFDEF SWINGAME_SDL13}sgDriverNetworkingSDL{$ELSE}sgDriverNetworkingSDL{$ENDIF};

  procedure LoadDefaultNetworkingDriver(); 
  begin
    {$IFDEF SWINGAME_SDL13}
      LoadSDLNetworkingDriver();
    {$ELSE}
      LoadSDLNetworkingDriver();
    {$ENDIF}
  end;

  function DefaultMyIPProcedure() : String;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.MyIP();
  end;

  function DefaultConnectionCountProcedure() : LongInt;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.ConnectionCount();
  end;

  function DefaultRetreiveConnectionProcedure(const aConnectionAt : LongInt) : Connection;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.RetreiveConnection(aConnectionAt);
  end;

//----------------------------------------------------------------------------
// TCP Connection Handling
//----------------------------------------------------------------------------

  function DefaultCreateTCPHostProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateTCPHost(aPort);
  end;

  function DefaultCreateTCPConnectionProcedure(const aDestIP : String; const aDestPort : LongInt) : Connection;
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateTCPConnection(aDestIP, aDestPort);
  end;  

  function DefaultAcceptTCPConnectionProcedure() : LongInt;
  begin  
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.AcceptTCPConnection();
  end;

//----------------------------------------------------------------------------
// TCP Message Handling
//----------------------------------------------------------------------------

  function DefaultTCPMessageReceivedProcedure() : Boolean;
   begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.TCPMessageReceived();
   end;

  function DefaultSendTCPMessageProcedure(const aMsg : String; const aConnection : Connection) : Connection;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.SendTCPMessage(aMsg, aConnection);
  end;

  procedure DefaultBroadcastTCPMessageProcedure(const aMsg : String);
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.BroadcastTCPMessage(aMsg);
  end;

//----------------------------------------------------------------------------
// UDP Connections
//----------------------------------------------------------------------------

  function DefaultCreateUDPHostProcedure(const aPort : LongInt) : LongInt;
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateUDPHost(aPort);
  end;
  
  function DefaultCreateUDPConnectionProcedure(const aDestIP : String; const aDestPort, aInPort : LongInt) : Connection; 
  begin    
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CreateUDPConnection(aDestIP, aDestPort, aInPort);
  end;

//----------------------------------------------------------------------------
// UDP Message
//----------------------------------------------------------------------------

  function DefaultUDPMessageReceivedProcedure() : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.UDPMessageReceived();
  end;
  
  function DefaultSendUDPMessageProcedure(const aMsg : String; const aConnection : Connection) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.SendUDPMessage(aMsg, aConnection);
  end;
	
	procedure DefaultBroadcastUDPMessageProcedure(const aMsg : String);
	begin
    LoadDefaultNetworkingDriver();
		NetworkingDriver.BroadcastUDPMessage(aMsg);
	end;

//----------------------------------------------------------------------------
// Close Single
//----------------------------------------------------------------------------

  function DefaultCloseTCPHostSocketProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseTCPHostSocket(aPort);
  end;

  function DefaultCloseConnectionProcedure(var aConnection : Connection; const aCloseUDPSocket : Boolean) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseConnection(aConnection, aCloseUDPSocket);
  end;

  function DefaultCloseUDPSocketProcedure(const aPort : LongInt) : Boolean;
  begin
    LoadDefaultNetworkingDriver();
    result := NetworkingDriver.CloseUDPSocket(aPort);
  end;

//----------------------------------------------------------------------------
// Close All
//----------------------------------------------------------------------------

  procedure DefaultCloseAllTCPHostSocketProcedure();
  begin    
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllTCPHostSocket();
  end;

  procedure DefaultCloseAllConnectionsProcedure(const aCloseUDPSockets : Boolean);
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllConnections(aCloseUDPSockets);
  end;

  procedure DefaultCloseAllUDPSocketProcedure();
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.CloseAllUDPSocket();
  end;  

  procedure DefaultFreeAllNetworkingResourcesProcedure();
  begin
    LoadDefaultNetworkingDriver();
    NetworkingDriver.FreeAllNetworkingResources();
  end;

  initialization 
  begin
    NetworkingDriver.CreateTCPHost               := @DefaultCreateTCPHostProcedure;
    NetworkingDriver.CreateTCPConnection     		 := @DefaultCreateTCPConnectionProcedure;
    NetworkingDriver.AcceptTCPConnection   			 := @DefaultAcceptTCPConnectionProcedure;
    NetworkingDriver.TCPMessageReceived          := @DefaultTCPMessageReceivedProcedure;
    NetworkingDriver.BroadcastTCPMessage         := @DefaultBroadcastTCPMessageProcedure;
    NetworkingDriver.SendTCPMessage	             := @DefaultSendTCPMessageProcedure;

    NetworkingDriver.CreateUDPHost               := @DefaultCreateUDPHostProcedure;
    NetworkingDriver.CreateUDPConnection         := @DefaultCreateUDPConnectionProcedure;
    NetworkingDriver.UDPMessageReceived          := @DefaultUDPMessageReceivedProcedure;
    NetworkingDriver.SendUDPMessage              := @DefaultSendUDPMessageProcedure;
		NetworkingDriver.BroadcastUDPMessage				 := @DefaultBroadcastUDPMessageProcedure;

    NetworkingDriver.CloseTCPHostSocket          := @DefaultCloseTCPHostSocketProcedure;
    NetworkingDriver.CloseConnection             := @DefaultCloseConnectionProcedure;
    NetworkingDriver.CloseUDPSocket        			 := @DefaultCloseUDPSocketProcedure;

    NetworkingDriver.MyIP                        := @DefaultMyIPProcedure;
		NetworkingDriver.ConnectionCount						 := @DefaultConnectionCountProcedure;
		NetworkingDriver.RetreiveConnection					 := @DefaultRetreiveConnectionProcedure;

    NetworkingDriver.CloseAllTCPHostSocket       := @DefaultCloseAllTCPHostSocketProcedure;     
    NetworkingDriver.CloseAllConnections         := @DefaultCloseAllConnectionsProcedure; 
    NetworkingDriver.CloseAllUDPSocket     			 := @DefaultCloseAllUDPSocketProcedure; 

    NetworkingDriver.FreeAllNetworkingResources  := @DefaultFreeAllNetworkingResourcesProcedure;
  end;
end.