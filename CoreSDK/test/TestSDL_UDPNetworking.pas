program TestSDL_NetClient;
uses
  sgNetworking, sgTypes, sgUtils;

const 
  LISTEN_PORTB = 49876;
  LISTEN_PORTA = LISTEN_PORTB + 1;

procedure Pause();
begin
  // Write('Press enter to continue.');
  // ReadLn();
  Delay(1000);
end;


procedure Main();
var
  svr: ServerSocket;
  toSvr : Connection = nil;
  lReceivedMsg : Boolean;
begin
  WriteLn('Listening for UDP connections on port ', LISTEN_PORTB);
  svr := CreateServer('MyServer', LISTEN_PORTB, UDP);
  Pause();

  WriteLn('Creating connection to send data to server');
  toSvr := OpenConnection('toSvr', '127.0.0.1', LISTEN_PORTB, UDP);
  Pause();

  WriteLn('Sending message to server');
  SendMessageTo('Hello UDP', toSvr);
  Pause();

  WriteLn('Checking activity');
  CheckNetworkActivity();
  Pause();
  CheckNetworkActivity();
  WriteLn('Server got message: ', HasMessages(svr));
  WriteLn('Message ', ReadMessageData(svr));
  Pause();

  CloseConnection(toSvr);

  WriteLn('Closing UDP socket on port ', LISTEN_PORTB);
  CloseServer(svr);
  Pause();

  Delay(3000);

  // WriteLn('Listening On: ', LISTEN_PORTB);

  // lConA := CreateUDPConnection('127.0.0.1', LISTEN_PORTB, LISTEN_PORTA); 

  // WriteLn('Created New Connection: ', HexStr(lConA));

  // ReadLn();

  // WriteLn('Sending Message to: ', LISTEN_PORTB);
  // SendUDPMessage('Hello Connection B', lConA);

  // ReadLn();

  // WriteLn('New Connection Queue Size: ', ConnectionQueueSize());
  // lReceivedMsg := UDPMessageReceived();
  // WriteLn('Received Message? ', lReceivedMsg);
  // WriteLn('New Connection Queue Size: ', ConnectionQueueSize());

  // lConB := FetchConnection();
  // WriteLn('lCon IP Dec Address: ', ConnectionPort(lConA));
  // WriteLn('Message From A to B: ', ReadMessage(lConB));
  // ReadLn();

  // WriteLn('Sending Message to: ', LISTEN_PORTA);
  // SendUDPMessage('Hey Connection A', lConB);
  // ReadLn();
  // lReceivedMsg := UDPMessageReceived();
  // WriteLn('Received Message? ', lReceivedMsg);
  // WriteLn('Message From B to A: ', ReadMessage(lConA));

  // WriteLn('Closing...');

  // CloseConnection(lConB);
  // CloseConnection(lConA);
  // WriteLn('Connections Closed');
  // CloseUDPListenSocket(LISTEN_PORTB);
  // CloseUDPListenSocket(LISTEN_PORTA);
  // WriteLn('Closed.');

  // WriteLn(HexStr(lConB));
  // WriteLn(HexStr(lConA));
  
  //  // ReleaseAllConnections();
end;

begin
	main();
end.