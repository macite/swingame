program TestUDPNetworking;
uses
  sgNetworking, sgTypes, sgUtils;

const 
  LISTEN_PORTB = 49876;
  LISTEN_PORTA = LISTEN_PORTB + 1;

procedure Pause();
begin
  Write('Press enter to continue.');
  ReadLn();
  // Delay(1000);
end;


procedure Main();
var
  svr: ServerSocket;
  toSvr: Connection = nil;
  toClient: Connection = nil;
  lReceivedMsg : Boolean;
  msg: Message;
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
  WriteLn('Server got message: ', HasMessages(svr));
  msg := ReadMessage(svr);
  WriteLn('Message ', MessageData(msg));
  Pause();

  WriteLn('Sending message to client ', MessageHost(msg), ':', MessagePort(msg));
  toClient := OpenConnection('toClient', MessageHost(msg), MessagePort(msg), UDP);
  CloseMessage(msg);

  WriteLn('Connection created ', Assigned(toClient));
  SendMessageTo('Hello Client', toClient);
  Delay(100);
  CheckNetworkActivity();

  WriteLn('Client got message ', HasMessages('toSvr'));
  WriteLn('Message ', ReadMessageData(toSvr));

  CloseConnection(toSvr);

  WriteLn('Closing UDP socket on port ', LISTEN_PORTB);
  CloseServer(svr);

  WriteLn('Close all');
  CloseAllConnections();
  CloseAllServers();
end;

begin
	Main();
end.