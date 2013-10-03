program HowToCreateTCPProgram;
uses
	SwinGame, SysUtils;

const
  HOST_PORT = 2000;

function SelectPeerType() : Boolean;
var 
  lInput : String = '';
begin
  Write('Are You Hosting? [y/n]: ');
  while (lInput <> 'y') and (lInput <> 'n') do
  begin
    ReadLn(lInput);
    if (lInput = 'y') then 
    begin
      CreateTCPHost(HOST_PORT); 
      result := True;    
			WriteLn('I am now the host');
    end else if (lInput = 'n') then
		begin
      result := False;  
			WriteLn('I am now the client');
		end;
  end;
end;

function WaitForConnections() : Connection;
begin  
  WriteLn('Waiting for Connections....');
  result := nil;
  while (result = nil) do
  begin
    AcceptTCPConnection();
    result := FetchConnection();
  end;
  WriteLn('Connection Established.');
end;

function WaitToConnectToHost() : Connection;
begin
  WriteLn('Connecting to Host....');
  result := nil;
  while (result = nil) do
    result := CreateTCPConnection('127.0.0.1', HOST_PORT);
  WriteLn('Connected to Host');
end;

procedure HandleMessages(const aPeer : Connection; aIsHost : Boolean);
var
  lMessage : String = '';
  i : Integer;
const 
  AUTO_MESSAGE_COUNT = 10;
  SEND_DELAY         = 1500;
begin
	for i := 0 to AUTO_MESSAGE_COUNT do
  begin
		if (aIsHost) then
			SendTCPMessage('Hello. This Message Number is from the Host. The client should receive it.', aPeer)
		else
			SendTCPMessage('Hello. This Message Number is from the Client. The host should receive it.', aPeer);
			
    Delay(SEND_DELAY);
		
    if TCPMessageReceived() then
      WriteLn('Received Message: ', ReadMessage(aPeer));
  end;
end;

procedure Main();
var
  lIsHost : Boolean = False;
  lPeer : Connection = nil;
begin
  lIsHost := SelectPeerType();

  if (lIsHost) then
    lPeer := WaitForConnections()
  else
    lPeer := WaitToConnectToHost();
  
  HandleMessages(lPeer, lIsHost);

  ReleaseAllResources();
end;

begin
  main();
end.