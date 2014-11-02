program HowToCreateHost;
uses
  SwinGame, sgTypes, sgNetworking, SysUtils;

//----------------------------------------------------------------------------
// Init
//----------------------------------------------------------------------------

procedure InitPanel(var aMenuPanel : Panel);
begin
  GUISetForegroundColor(ColorBlack);
  GUISetBackgroundColor(ColorWhite);
  ShowPanel(aMenuPanel);
  ActivatePanel(aMenuPanel);
  DrawGUIAsVectors(True);
end;

procedure SetLabelText();
begin
  LabelSetText('MyIPLbl', 'My IP:');
  LabelSetText('MyIPVal', MyIP());
  LabelSetText('MyPorttLbl', 'Port: ');
  LabelSetText('HostStatusLbl', 'Status: ');

  LabelSetText('MsgsLbl', 'Message: ');

  LabelSetText('ServerLbl', 'Server:');
  LabelSetText('ServerPorttLbl', 'Port: ');
  LabelSetText('ClientStatusLbl', 'Status: ');
  LabelSetText('ServerVal', '127.0.0.1');
end;

//----------------------------------------------------------------------------
// Connect
//----------------------------------------------------------------------------

procedure CreateHost(var aMenuPanel : Panel; var aStatus : Boolean);
var
  lPort : Integer;
begin
  if TryStrToInt(TextBoxText('MyPortVal'), lPort) then
  begin
    CreateTCPHost(lPort);
    LabelSetText('HostStatusVal', 'Listening...');
    LabelSetText('HostLbl', 'Disconnect');
    ListAddItem(aMenuPanel, 'MsgList',  'Now Listening for Connections....');
    aStatus := True;
  end;
end;

procedure ConnectToHost(aMenuPanel : Panel; var aStatus : Boolean; var lPeer : Connection);
var
  lPort : Integer;
begin
  if TryStrToInt(TextBoxText('ServerPortVal'), lPort) then
  begin
		lPeer := CreateTCPConnection(TextBoxText('ServerVal'), lPort);
    if Assigned(lPeer) then
    begin
      LabelSetText('ClientStatusVal', 'Connected');
      ListAddItem(aMenuPanel, 'MsgList', 'Connected to: ' + TextBoxText('ServerVal') + ':' + IntToStr(lPort));
      LabelSetText('ConnLbl', 'Disconnect');
      aStatus := True;
    end;
  end;
end;

procedure AcceptConnection(var aMenuPanel : Panel; var lPeer : Connection);
begin
  if AcceptTCPConnection(); then
  begin
		lPeer := FetchConnection();
    ListAddItem(aMenuPanel, 'MsgList',  + 'Connected.');
  end;
end;

//----------------------------------------------------------------------------
// Disconnect
//----------------------------------------------------------------------------

procedure DisconnectHost(var aMenuPanel : Panel; var aStatus : Boolean);
begin
  CloseAllTCPReceiverSocket();
  CloseAllTCPHostSocket();
  LabelSetText('HostStatusVal', 'Idle');
  LabelSetText('HostLbl', 'Connect');
  ListAddItem(aMenuPanel, 'MsgList',  'Listening Sockets Closed.');
  aStatus := False;
end;

procedure DisconnectFromAllHost(aMenuPanel : Panel; var aStatus : Boolean);
begin
  BroadcastTCPMessage('[ClientDisconnect]');
  CloseAllTCPSenderSocket();
  LabelSetText('ClientStatusVal', 'Disconnected');
  LabelSetText('ConnLbl', 'Connect');
  ListAddItem(aMenuPanel, 'MsgList', 'You Disconnected.');
  aStatus := False;
end;

//----------------------------------------------------------------------------
// Messages
//----------------------------------------------------------------------------

procedure ReceiveMessage(var aMenuPanel : Panel);
var
  i : Integer;
begin  
  if (not TCPMessageReceived()) then exit;
  for i:= 0 to GetMessageCount() - 1 do
  begin
    if (GetMessage() = '[ClientDisconnect]') then
    begin
      CloseTCPReceiverSocket(GetIPFromMessage(), GetPortFromMessage());
      ListAddItem(aMenuPanel, 'MsgList',  GetIPFromMessage() + ' Disconnected.');
    end else begin
      ListAddItem(aMenuPanel, 'MsgList',  GetIPFromMessage() + ' said: ' + GetMessage());
      ListSetActiveItemIndex(aMenuPanel, 'MsgList', ListItemCount('MsgList') - 1);
    end;
    DequeueTopMessage();
  end;
end;

//----------------------------------------------------------------------------
// Input
//----------------------------------------------------------------------------

procedure HandleGUIInput(aChatPanel : Panel; var aServerStatus, aClientStatus : Boolean);
begin
  //Server Connect and Disconnect
  if (not aServerStatus) and (RegionClickedID() = 'HostBtn') then
    CreateHost(aChatPanel, aServerStatus)
  else if (aServerStatus) and (RegionClickedID() = 'HostBtn') then
    DisconnectHost(aChatPanel, aServerStatus)
  //Client Connect and Disconnect
  else if (not aClientStatus) and (RegionClickedID() = 'ConnectBtn') then
    ConnectToHost(aChatPanel, aClientStatus)
  else if (aClientStatus) and (RegionClickedID() = 'ConnectBtn') then
    DisconnectFromAllHost(aChatPanel, aClientStatus)
  //Message Input
  else if GUITextEntryComplete() and (GUITextBoxOfTextEntered() = TextBoxFromID('MsgVal')) then
  begin
    if (TextBoxText('MsgVal') = '') then exit;
    if (Length(BroadcastTCPMessage(TextBoxText('MsgVal'))) <> 0) then
    begin
      ListAddItem(aChatPanel, 'MsgList', 'Host Disconnected.');      
      DisconnectFromAllHost(aChatPanel, aClientStatus);
      exit;
    end;
    ListAddItem(aChatPanel, 'MsgList', 'You Say: ' + TextBoxText('MsgVal'));
    ListSetActiveItemIndex(aChatPanel, 'MsgList', ListItemCount('MsgList') - 1);
    TextBoxSetText('MsgVal', '');
    GUISetActiveTextbox(TextBoxFromID('MsgVal'));
  end;
end;

procedure Main();
var
  lServerPanel, lClientPanel, lChatPanel : Panel;
  //True for Listening, False for Idle.
  lHostStatus : Boolean = False;
  //True for Connected, False for Disconnected
  lClientStatus : Boolean = False; 
	lPeer : Connection = nil;
begin
  OpenGraphicsWindow('How To Create Host', 800, 600); 
  LoadDefaultColors();

  LoadResourceBundle('MainMenu.txt');
  lServerPanel := LoadPanel('hostpanel.txt');
  lClientPanel := LoadPanel('clientpanel.txt');
  lChatPanel := LoadPanel('ChatWindowPanel.txt');
  InitPanel(lServerPanel);
  InitPanel(lClientPanel);
  InitPanel(lChatPanel);
  SetLabelText();

  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    UpdateInterface();
    HandleGUIInput(lChatPanel, lHostStatus, lClientStatus);
		if not lHostStatus then
			AcceptConnection(lChatPanel, lPeer);
		else
			ConnectToHost(aMenuPanel : Panel; var aStatus : Boolean; var lPeer : Connection);
    ReceiveMessage(lChatPanel);

    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
end;

begin
  Main();
end.