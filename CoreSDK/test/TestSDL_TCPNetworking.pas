program TestSDL_NetServer;
uses
  sgNetworking, sgTypes, sgUtils, sgDriverSDL2Types;

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
  var
    i: Integer;
    msg: Message;
  begin
    WriteLn('Checking for messages');
    CheckNetworkActivity();

    if HasMessages() then
    begin
      WriteLn(' Reading messages received for client');

      while HasMessages(lConA) do
      begin
        msg := ReadMessage(lConA);
        WriteLn(' -> ', msg.data, ' from ', msg.host, ':', msg.port);
      end;

      WriteLn(' Reading messages received for ToSvr client');

      while HasMessages('ToSvr') do
      begin
        msg := ReadMessage('ToSvr');
        WriteLn(' -> ', msg.data, ' from ', msg.host, ':', msg.port);
      end;

      WriteLn(' Reading messages received for server');

      // while HasMessages(lConB) do
      // begin
      //   WriteLn(' -> ', ReadMessageData(lConB));
      // end;

      for i := 0 to ConnectionCount(svr) - 1 do
      begin
        lTmp := RetreiveConnection(svr, i);
        if HasMessages(lTmp) then
          WriteLn(' -> ', ReadMessageData(lTmp) , ' from ', ConnectionPort(lTmp));
      end;

      Pause();
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

  WriteLn('Listening on ', SVR1_PORT, ' and ', SVR2_PORT, '.');
  Pause();

  WriteLn('Attempting to open connection twice (should fail): ', Assigned(CreateServer('svr3', SVR2_PORT)));

  WriteLn('Connecting to Port (x2) ', SVR1_PORT);
  lConA := OpenConnection('127.0.0.1', SVR1_PORT);
  OpenConnection('ToSvr', '127.0.0.1', SVR1_PORT);

  WriteLn('ToSvr is open: ', ConnectionOpen('ToSvr') );
  WriteLn('Fred is open: ', ConnectionOpen('Fred') );

  CheckNetworkActivity();

  WriteLn('Are there new connections? ', HasNewConnections());
  WriteLn('New Connection to ', SVR1_PORT, ': ', ServerHasNewConnection(svr));
  WriteLn(' Number of connections: ', ConnectionCount(svr));

  lConB := LastConnection(svr);

  CheckNetworkActivity();
  WriteLn('Are there new connections? ', HasNewConnections());
  WriteLn('New Connection to ', SVR1_PORT, ': ', ServerHasNewConnection(svr));
  WriteLn(' Number of connections: ', ConnectionCount(svr));

  Pause();
  WriteLn('Checking for messages -- shouldn''t be any');
  CheckMessages();

  WriteLn('Sending messages');
  SendMessageTo('To server --> from client', lConA);
  SendMessageTo('To server --> from named client', 'ToSvr');
  SendMessageTo(StringOfChar('7', 509 - 4), lConA);
  SendMessageTo('1234567', lConA);
  SendMessageTo('0987654', lConA);
  SendMessageTo(StringOfChar('A', 876), lConA);

  SendMessageTo(StringOfChar('7', 509 - 4), lConB);
  SendMessageTo('Hello Client', lConB);
  SendMessageTo(StringOfChar('A', 876), lConB);

  Pause();
  CheckMessages();
  CheckMessages();
  CheckMessages();

  WriteLn('Closing client - ', CloseConnection(lConA));
  Pause();

  WriteLn('Client still: ', Assigned(lConA));
  WriteLn('Test message send (to closed client): ', SendMessageTo(StringOfChar('A', 876), lConB));

  Pause();
  WriteLn('Test message send (expect false): ', SendMessageTo(StringOfChar('A', 876), lConB));

  WriteLn('Server still connected to client: ', ConnectionOpen(RetreiveConnection(svr, 0)));

  WriteLn('Closing server''s client connection (should already by closed -- just testing): ', CloseConnection(lConB));
  WriteLn('Server connections: ', ConnectionCount(svr));
  Pause();

  WriteLn('Checking for activity');
  CheckNetworkActivity();

  WriteLn('Opening a new connection');

  lConA := OpenConnection('127.0.0.1', SVR1_PORT);
  WriteLn('Reconnected: ', Assigned(lConA));

  CheckNetworkActivity();
  lConB := LastConnection(svr);
  WriteLn('Connections = ', ConnectionCount(svr));

  SendMessageTo('New connection --> to server', lConA);
  SendMessageTo('New connection --> to client', lConB);

  CheckMessages();

  WriteLn('Closing server: ', CloseServer(svr));
  Pause();

  WriteLn('Can connect to old server? ', Assigned(OpenConnection('127.0.0.1', SVR1_PORT)));
  Pause();

  WriteLn('Restarting server...');
  svr := CreateServer('svr1', SVR1_PORT);

  Reconnect(lConA);
  Reconnect('ToSvr');

  CheckNetworkActivity();
  lConB := LastConnection(svr);

  CheckNetworkActivity();

  BroadcastMessage('Hello Everyone');
  BroadcastMessage('Hello Everyone on svr', svr);

  SendMessageTo('Another message --> to server', lConA);
  SendMessageTo('Another message --> to client', lConB);
  SendMessageTo('Another message --> from named client', 'ToSvr');
  SendMessageTo('Another message --> to named client', LastConnection(svr));

  CheckMessages();
  CheckMessages();

  WriteLn('Close all');
  CloseAllConnections();
  CloseAllServers();
  Pause();
end;

begin
	Main();
end.
