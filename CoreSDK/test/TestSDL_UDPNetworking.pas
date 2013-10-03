program TestSDL_NetClient;
uses
  sgNetworking, sgTypes, sgUtils;

const 
  LISTEN_PORTB = 2000;
  LISTEN_PORTA = 2001;

procedure Main();
var
  lConA : Connection = nil;
  lConB : Connection = nil;
  lReceivedMsg : Boolean;
begin
  CreateUDPSocket(LISTEN_PORTB);

  WriteLn('Listening On: ', LISTEN_PORTB);

  lConA := CreateUDPConnection('127.0.0.1', LISTEN_PORTB, LISTEN_PORTA); 

  WriteLn('Created New Connection: ', HexStr(lConA));

  ReadLn();

  WriteLn('Sending Message to: ', LISTEN_PORTB);
  SendUDPMessage('Hello Connection B', lConA);

  ReadLn();

  WriteLn('New Connection Queue Size: ', ConnectionQueueSize());
  lReceivedMsg := UDPMessageReceived();
  WriteLn('Received Message? ', lReceivedMsg);
  WriteLn('New Connection Queue Size: ', ConnectionQueueSize());

  lConB := FetchConnection();
  WriteLn('lCon IP Dec Address: ', ConnectionPort(lConA));
  WriteLn('Message From A to B: ', ReadMessage(lConB));
  ReadLn();

  WriteLn('Sending Message to: ', LISTEN_PORTA);
  SendUDPMessage('Hey Connection A', lConB);
  ReadLn();
  lReceivedMsg := UDPMessageReceived();
  WriteLn('Received Message? ', lReceivedMsg);
  WriteLn('Message From B to A: ', ReadMessage(lConA));

  WriteLn('Closing...');

  CloseConnection(lConB);
  CloseConnection(lConA);
  WriteLn('Connections Closed');
  CloseUDPListenSocket(LISTEN_PORTB);
  CloseUDPListenSocket(LISTEN_PORTA);
  WriteLn('Closed.');

  WriteLn(HexStr(lConB));
  WriteLn(HexStr(lConA));
  
   // ReleaseAllConnections();
end;

begin
	main();
end.