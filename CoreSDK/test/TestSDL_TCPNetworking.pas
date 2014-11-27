program TestSDL_NetServer;
uses
  sgNetworking, sgTypes, sgUtils;

procedure Pause();
begin
  Write('Press enter to continue.');
  ReadLn();
end;

const 
  SVR1_PORT = 49876;
  SVR2_PORT = SVR1_PORT + 1; 

procedure Main();
var
  svr: ServerSocket;
  lConA, lConB, lTmp, lTmpA : Connection;
  lMsgReceived : Boolean = False;

  procedure CheckMessages();
  begin
    WriteLn('Checking for messages');
    CheckNetworkActivity();

    if MessagesReceived() then
    begin
      WriteLn(' Reading messages received for client');

      while HasMessages(lConA) do
      begin
        WriteLn(' -> ', ReadMessage(lConA));
      end;

      WriteLn(' Reading messages received for server');

      while HasMessages(lConB) do
      begin
        WriteLn(' -> ', ReadMessage(lConB));
      end;    
    end
    else
    begin
      WriteLn('No messages received');
    end;
  end;

begin
  WriteLn('Starting');

  svr := CreateServer('svr1', SVR1_PORT);
  CreateServer('svr2', SVR2_PORT);
  // IPv4ToDec('127.0.0.1');

  WriteLn('Listening on ', SVR1_PORT, ' and ', SVR2_PORT, '.');
  Pause();

  WriteLn('Attempting to open connection twice: ', Assigned(CreateServer('svr3', SVR2_PORT)));

  WriteLn('Connecting to Port ', SVR1_PORT);
  lConA := OpenConnection('127.0.0.1', SVR1_PORT);
  
  CheckNetworkActivity();
  
  WriteLn('Are there new connections? ', HasNewConnections());
  WriteLn('New Connection to ', SVR1_PORT, ': ', ServerHasNewConnection(svr));
  WriteLn(' Number of connections: ', ConnectionCount(svr));

  lConB := LastConnection(svr);

  WriteLn('Connection Retreived Successfully? : ', Assigned(lConB));

  Pause();
  WriteLn('Checking for messages -- shouldn''t be any');
  CheckMessages();

  Pause();
  WriteLn('Sending messages');
  SendMessageTo(StringOfChar('7', 509 - 4), lConA);
  SendMessageTo('1234567', lConA);
  SendMessageTo('0987654', lConA);
  SendMessageTo(StringOfChar('A', 876), lConA);

  SendMessageTo(StringOfChar('7', 509 - 4), lConB);
  SendMessageTo('Hello Client', lConB);
  SendMessageTo(StringOfChar('A', 876), lConB);

  Pause();
  CheckMessages();

  Pause();
  WriteLn('Closing client - ', CloseConnection(lConA));
  Pause();

  WriteLn('Client still: ', Assigned(lConA));
  WriteLn('Test message send (to closed client): ', SendMessageTo(StringOfChar('A', 876), lConB));
  // WriteLn('Test message send (expect false): ', SendMessageTo('Hello Client', lConA));

  Pause();
  WriteLn('Test message send (expect false): ', SendMessageTo(StringOfChar('A', 876), lConB));
  // WriteLn('Test message send (expect false): ', SendMessageTo('Hello Client', lConA));

  WriteLn('Server still connected to client: ', ConnectionOpen(RetreiveConnection(svr, 0)));

  WriteLn('Closing server'' client connection: ', CloseConnection(lConB));
  WriteLn('Server connections: ', ConnectionCount(svr));
  Pause();

  WriteLn('Opening a new connection');

  lConA := OpenConnection('127.0.0.1', SVR1_PORT);
  WriteLn('Reconnected: ', Assigned(lConA));

  CheckNetworkActivity();
  lConB := LastConnection(svr);
  WriteLn('Connections = ', ConnectionCount(svr));

  SendMessageTo('New connection --> to server', lConA);
  SendMessageTo('New connection --> to client', lConB);

  CheckMessages();

  Pause();
  WriteLn('Closing server: ', CloseServer(svr));
  Pause();

  WriteLn('Can connect to old server? ', Assigned(OpenConnection('127.0.0.1', SVR1_PORT)));
  Pause();

  WriteLn('Restarting server...');
  svr := CreateServer('svr1', SVR1_PORT);

  ReconnectConnection(lConA);
  CheckNetworkActivity();
  lConB := LastConnection(svr);

  BroadcastMessage('Hello Everyone');
  BroadcastMessage('Hello Everyone on svr', svr);

  SendMessageTo('Another message --> to server', lConA);
  SendMessageTo('Another message --> to client', lConB);

  CheckMessages();
  Pause();

  WriteLn('Close all');
  CloseAllConnections();
  CloseAllServers();
  Pause();


  // Read the message from the server...
  // WriteLn('Message was received by svr: ', ServerHasMessages(svr));

  // WriteLn('Con A Address: ', HexStr(@lConA^) );
  // WriteLn('Con B Address: ', HexStr(@lConB^) );
 
  // ReadLn();
 

  // lMsgReceived := TCPMessageReceived();
  // WriteLn('Message Received? ', lMsgReceived);
  // WriteLn('Message From [A], Received By [B]: ', ReadMessage(lConB));
  // WriteLn('Message From [B], Received By [A]: ', ReadMessage(lConb));
  // lMsgReceived := TCPMessageReceived();
  // WriteLn('Message Received? ', lMsgReceived);
  // WriteLn('Message From [A], Received By [B]: ', ReadMessage(lConB));

  // CloseConnection(lConB);
  // CloseConnection(lConA);
  // WriteLn('Connections Closed');
  // CloseTCPHostSocket(2000);
end;

begin
	Main();
end.
