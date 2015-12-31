program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  SwinGame, sgTypes;


procedure __callback__Button1Clicked(r: Region; kind: EventKind);
begin
  WriteLn('here button1 - ', RegionID(r), ' - ', Longint(kind));
  LabelSetText('Label1', TextBoxText('TextBox1'));
end;

procedure __callback__List1(r: Region; kind: EventKind);
begin
  WriteLn('here - list1 - ', RegionID(r), ' - ', Longint(kind));
  if kind = ekSelectionMade then
  begin
    LabelSetText('Label1', ListActiveItemText('List1'));
  end;
end;

  
procedure InitInterface(var pnla, pnlb: Panel);
begin
  GUISetForegroundColor(RGBAColor(128,128,128,127));
  GUISetBackgroundColor(ColorWhite);
  writeln('colors set');
  pnla := LoadPanel('panelwithbutton.txt');
  pnlb := LoadPanel('panelwithlabel.txt');
  
  ShowPanel(pnla);
  HidePanel(pnlb);
  
  ActivatePanel(pnla);
  DeactivatePanel(pnlb);
  
  DrawGUIAsVectors(false);
  
  RegisterEventCallback(RegionWithId(pnla, 'Button1'), @__callback__Button1Clicked);
  RegisterEventCallback(RegionWithId(pnla, 'List1'), @__callback__List1);
end;


procedure UpdateGUI(var pnla, pnlb: Panel);
begin
  // if (RegionClickedID() = 'Button1') then
  // begin
  //  LabelSetText(LabelFromRegion(RegionWithID('Label1')), TextBoxText(RegionWithID('TextBox1')));
  // end;

  if CheckboxState(RegionWithID('Checkbox1')) then
  begin
    DrawGUIAsVectors(false)
  end
  else
    DrawGUIAsVectors(true);
    
  if CheckboxState(RegionWithID('Checkbox2')) then
    PanelSetDraggable(pnla, true)
  else
    PanelSetDraggable(pnla, false);
end;
  
procedure Main();
var
	pnla, pnlb: Panel;
begin
  OpenGraphicsWindow('Hello World', 800, 600);
  
  //ToggleFullScreen();
  
  LoadResourceBundle('MainMenu.txt');

  InitInterface(pnla,pnlb);

  ListAddItem('List1', BitmapNamed('hat'),'Hat');
  ListAddItem('List1', BitmapNamed('sword'),'Sword');
  ListAddItem('List1', BitmapNamed('cape'),'Cape');
  ListAddItem('List1', 'Cheese');
  ListAddItem('List1', 'Cake');
  ListAddItem('List1', 'Mouse');
  ListAddItem('List1', 'Dog');
  ListAddItem('List1', 'Axe');
  ListAddItem('List1', 'Mace');
  ListAddItem('List1', 'Chainmail');
  ListAddItem('List1', 'Ninja');
  ListAddItem('List1', 'Newspaper');
  ListAddItem('List1', 'Car');  
  ListAddItem('List1', 'Hat2');
  ListAddItem('List1', 'Sword2');
  ListAddItem('List1', 'Cape2');
  ListAddItem('List1', 'Cheese2');
  ListAddItem('List1', 'Cake2');
  ListAddItem('List1', 'Mouse2');
  ListAddItem('List1', 'Dog2');
  ListAddItem('List1', 'Axe2');
  ListAddItem('List1', 'Mace2');
  ListAddItem('List1', 'Chainmail2');
  ListAddItem('List1', 'Ninja2');
  ListAddItem('List1', 'Newspaper2');
  ListAddItem('List1', 'Car2');
  
  
  repeat
    ProcessEvents();
    ClearScreen(ColorGreen);
    
    DrawInterface();
    UpdateGUI(pnla, pnlb);
    UpdateInterface();
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
