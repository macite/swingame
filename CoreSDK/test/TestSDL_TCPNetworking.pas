program TestSDL_NetServer;
uses
  sgNetworking, sgTypes, sgUtils;

procedure Pause();
begin
  Write('Press enter to continue.');
  ReadLn();
end;

procedure Main();
var
  svr: ServerSocket;
  lConA, lConB, lTmp, lTmpA : Connection;
  lMsgReceived : Boolean = False;
begin
  svr := CreateTCPServer('svr2000', 2000);
  CreateTCPServer('svr2001', 2001);
  // IPv4ToDec('127.0.0.1');

  WriteLn('Listening on 2000 and 2001.');
  Pause();

  WriteLn('Connecting to Port 2000');
  lConA := OpenTCPConnection('127.0.0.1', 2000);
  
  // WriteLn('Connection Queue Size: ', ConnectionQueueSize());
  // WriteLn('New Connections to 2001: ', AcceptNewConnection('svr2001'));
  WriteLn('New Connection to 2000: ', AcceptNewConnection(svr));
  WriteLn('2000 connections: ', ConnectionCount(svr));

  lConB := LastConnection(svr);

  // CloseTCPHostSocket(2001);      

  // WriteLn('Connection Queue Size: ', ConnectionQueueSize());
  WriteLn('Connection Retreived Successfully? : ', Assigned(lConB));

  Pause();
  WriteLn('Checking for messages -- shouldn''t be any');
  CheckNetworkActivity();
  WriteLn('');

  Pause();

  WriteLn('Sending messages');
  SendMessageTo('1234567', lConA);
  SendMessageTo('0987654', lConA);
  SendMessageTo(StringOfChar('A', 255), lConA);

  Pause();
  WriteLn('Checking for messages');
  CheckNetworkActivity();

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
