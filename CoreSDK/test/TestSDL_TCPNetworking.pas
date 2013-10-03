program TestSDL_NetServer;
uses
  sgNetworking, sgTypes, sgUtils;

procedure Main();
var
  lConA, lConB, lTmp, lTmpA : Connection;
  lMsgReceived : Boolean = False;
begin
  CreateTCPHost(2000);      
  IPv4ToDec('127.0.0.1');
  WriteLn('Listening on 2000 and 2001');

  ReadLn();
  //Connect to Port 2000, Listen on 2001
  lConA := OpenTCPConnectionToHost('127.0.0.1', 2000);
  
  WriteLn('Connection Queue Size: ', ConnectionQueueSize());
  WriteLn('New Connections : ', ServerAcceptTCPConnection());
  WriteLn('Connection Queue Size: ', ConnectionQueueSize());

  lConB := FetchConnection();
  CloseTCPHostSocket(2001);      

  WriteLn('Connection Queue Size: ', ConnectionQueueSize());
  WriteLn('Connection Retreived Successfully? : ', Assigned(lConB));

  ReadLn();
  SendTCPMessageTo('1234567', lConA);
  SendTCPMessageTo('0987654', lConA);
  SendTCPMessageTo('ABCDEFG', lConA);

  WriteLn('Con A Address: ', HexStr(@lConA^) );
  WriteLn('Con B Address: ', HexStr(@lConB^) );
 
  ReadLn();
 
  lMsgReceived := TCPMessageReceived();
  WriteLn('Message Received? ', lMsgReceived);
  WriteLn('Message From [A], Received By [B]: ', ReadMessage(lConB));
  WriteLn('Message From [B], Received By [A]: ', ReadMessage(lConb));
  lMsgReceived := TCPMessageReceived();
  WriteLn('Message Received? ', lMsgReceived);
  WriteLn('Message From [A], Received By [B]: ', ReadMessage(lConB));

  CloseConnection(lConB);
  CloseConnection(lConA);
  WriteLn('Connections Closed');
  CloseTCPHostSocket(2000);
end;

begin
	main();
end.