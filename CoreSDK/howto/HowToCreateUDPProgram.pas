program HowToCreateUDPProgram;
uses
	SwinGame, SysUtils;

const
  PEER_A_PORT = 2000;
  PEER_B_PORT = 2001;

function SelectPeerType() : Connection;
var 
  lInput : String = '';
begin
  Write('Are You Hosting? [y/n]: ');
  while (lInput <> 'y') and (lInput <> 'n') do
  begin
    ReadLn(lInput);
    if (lInput = 'y') then 
    begin
      CreateUDPHost(PEER_A_PORT); 
      result := nil;    
			WriteLn('I am now the host');
    end else if (lInput = 'n') then
		begin
			result := CreateUDPConnection('127.0.0.1', PEER_A_PORT, PEER_B_PORT);
			SendUDPMessage('Accept Me', result);
			WriteLn('I am now the client');
		end;
  end;
end;

procedure AcceptConnection(var aPeer : Connection);
begin
  while not Assigned(aPeer) do
  begin
		if UDPMessageReceived() then
		begin
			aPeer := FetchConnection();
			WriteLn('Host Received Message: ', ReadMessage(aPeer));
		end;
	end;
end;


procedure HandleMessages(var aPeer : Connection; const aIsHost : Boolean);
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
			SendUDPMessage('Hello. This Message is from the Host. The client should receive it.', aPeer)
		else
			SendUDPMessage('Hello. This Message is from the Client. The host should receive it.', aPeer);
			
    Delay(SEND_DELAY);
		
    if UDPMessageReceived() then
      WriteLn('Received Message: ', ReadMessage(aPeer));
  end;
end;

procedure Main();
var
  lIsHost : Boolean = False;
  lPeer : Connection = nil;
begin
  lPeer := SelectPeerType();
  lIsHost := not Assigned(lPeer);
	if lIsHost then AcceptConnection(lPeer);
	
  HandleMessages(lPeer, lIsHost);

  CloseAllConnections();
end;

begin
  main();
end.