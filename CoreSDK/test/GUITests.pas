program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils, sgImages, sgInput;


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

  
procedure InitInterface(var pnla, pnlb: panel);
begin
  GUISetForegroundColor(RGBAColor(128,128,128,127));
  GUISetBackgroundColor(ColorTransparent);
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


procedure UpdateGUI(var pnla, pnlb: panel; lst: GUilist);
var
  radGroup: GUIRadioGroup;
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
    
  radGroup := RadioGroupFromRegion(RegionWithID('radButton1'));
end;
  
procedure Main();
var
	pnla, pnlb: Panel;
	lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('Hello World', 800, 600);
  
  //ToggleFullScreen();
  
  LoadResourceBundle('MainMenu.txt');
  InitInterface(pnla,pnlb);
  lst := ListFromRegion(regionWithID('List1'));
  ListAddItem(lst, BitmapNamed('hat'),'Hat');
  ListAddItem(lst, BitmapNamed('sword'),'Sword');
  ListAddItem(lst, BitmapNamed('cape'),'Cape');
  ListAddItem(lst, 'Cheese');
  ListAddItem(lst, 'Cake');
  ListAddItem(lst, 'Mouse');
  ListAddItem(lst, 'Dog');
  ListAddItem(lst, 'Axe');
  ListAddItem(lst, 'Mace');
  ListAddItem(lst, 'Chainmail');
  ListAddItem(lst, 'Ninja');
  ListAddItem(lst, 'Newspaper');
  ListAddItem(lst, 'Car');  
  ListAddItem(lst, 'Hat2');
  ListAddItem(lst, 'Sword2');
  ListAddItem(lst, 'Cape2');
  ListAddItem(lst, 'Cheese2');
  ListAddItem(lst, 'Cake2');
  ListAddItem(lst, 'Mouse2');
  ListAddItem(lst, 'Dog2');
  ListAddItem(lst, 'Axe2');
  ListAddItem(lst, 'Mace2');
  ListAddItem(lst, 'Chainmail2');
  ListAddItem(lst, 'Ninja2');
  ListAddItem(lst, 'Newspaper2');
  ListAddItem(lst, 'Car2');
  
  
  repeat
    ProcessEvents();
    ClearScreen(ColorGreen);
    
    DrawInterface();
    UpdateGUI(pnla, pnlb, lst);
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
