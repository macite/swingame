program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils, sgImages, sgNamedIndexCollection;
  
type
  ArrayOfPanel = Array of Panel;
  
var
  panels: ArrayOfPanel;
  workingPanel: Panel;
  
const
  Tabs = 0;
  EditPanel = 1;
  EditRegion = 2;
  LowerInfo = 3;
  EditButton = 4;
  EditLabel = 5;
  EditCheckBox = 6;
  EditRadioGroup = 7;
  EditTextBox = 8;
  EditList = 9;
  
procedure SetupColors();
begin
  GUISetBackgroundColorInactive(ColorWhite);
  GUISetForegroundColorInactive(ColorGrey);
  
  GUISetForegroundColor(ColorBlack);
  GUISetBackgroundColor(ColorWhite);
end;
  
procedure UpdateWorkingPanel();
var
  testInt: Longint;
begin
  
  testInt := 10;
  if TryStrToInt(TextBoxText(RegionWithID('PanelWidth')), testInt) then
    workingPanel^.area.width  := testInt;

    

  testInt := 10;
  if TryStrToInt(TextBoxText(RegionWithID('PanelHeight')), testInt) then
    workingPanel^.area.height := testInt;

  

  workingPanel^.panelBitmap         := BitmapNamed(TextBoxText(RegionWithID('ImageS')));
  workingPanel^.panelBitmapInactive := BitmapNamed(TextBoxText(RegionWithID('ImageI')));
  workingPanel^.panelBitmapActive   := BitmapNamed(TextBoxText(RegionWithID('ImageA')));
    
  // Updating the panel for exporting.
    
  
end;

procedure InitInterface();
var
  i: Longint;
begin
  SetLength(panels, 10);
  panels[Tabs] := LoadPanel('IETabs.txt');
  panels[EditPanel] := LoadPanel('IEpanelPanel.txt');
  panels[EditRegion] := LoadPanel('IEregionPanel.txt');
  panels[LowerInfo] := LoadPanel('IElowerInfoPanel.txt');
  panels[EditButton] := LoadPanel('IEbuttonPanel.txt');
  panels[EditCheckBox] := LoadPanel('IEcheckboxPanel.txt');
  panels[EditRadioGroup] := LoadPanel('IEradiogroupPanel.txt');
  panels[EditLabel] := LoadPanel('IElabelPanel.txt');
  panels[EditTextBox] := LoadPanel('IEtextboxPanel.txt');
  panels[EditList] := LoadPanel('IElistPanel.txt');
  
  for i := 0 to 3 do
  begin
    ShowPanel(panels[i]);
  end;
  
  for i := 4 to High(panels) do
  begin
    HidePanel(panels[i]);
  end;
  
  SetupColors();
end;
  
procedure AddRegionToPanel();
var
  workingRegion: String;
  kinds: GUIRadioGroup;
begin
  workingRegion   := '';
  workingRegion   += (TextBoxText(RegionWithID('TBRegionX')) + ',');
  workingRegion   += (TextBoxText(RegionWithID('TBRegionY')) + ',');
  workingRegion   += (TextBoxText(RegionWithID('TBRegionWidth')) + ',');
  workingRegion   += (TextBoxText(RegionWithID('TBRegionHeight')) + ',');
  
  kinds := RadioGroupFromId(panels[EditRegion], 'KindRadio');
    
  case ActiveRadioButtonIndex(kinds) of
      0:  WorkingRegion += 'Button';
      1:  WorkingRegion += 'Button';
      2:  WorkingRegion += 'Button';
      3:  WorkingRegion += 'Button';
      4:  WorkingRegion += 'Button';
      5:  WorkingRegion += 'Button';
  end;
  
  workingRegion += (',' + TextBoxText(RegionWithID('TBRegionName')));
  
  AddRegionToPanelWithString(workingRegion, workingPanel);  
end;

procedure MyUpdateGUI();
var
  tab, kinds: GUIRadioGroup;
  i: Longint;
begin
  tab := RadioGroupFromId(panels[Tabs], 'UpperTabs');
  
  case ActiveRadioButtonIndex(tab) of
    0: begin ShowPanel(panels[EditPanel]); HidePanel(panels[EditRegion]); end;
    1: begin ShowPanel(panels[EditRegion]); HidePanel(panels[EditPanel]); end;
  end;
  
  kinds := RadioGroupFromId(panels[EditRegion], 'KindRadio');
  
  
  for i := Low(kinds^.buttons) to High(kinds^.buttons) do
  begin
    if i = ActiveRadioButtonIndex(kinds) then
    begin
      case ActiveRadioButtonIndex(kinds) of
        0:  ShowPanel(panels[EditButton]);
        1:  ShowPanel(panels[EditLabel]);
        2:  ShowPanel(panels[EditCheckBox]);
        3:  ShowPanel(panels[EditRadioGroup]);
        4:  ShowPanel(panels[EditTextbox]);
        5:  ShowPanel(panels[EditList]);
      end;
    end
    else
    begin
      //WriteLn('Found inactive');
      HidePanel(panels[i + 4]); //Offset the hidden panel index to skip the first 3 panels in the array.
    end;
  end;

  if RegionClickedID() = 'AddRegionButton' then
    AddRegionToPanel();
    
  UpdateWorkingPanel();
end;

procedure InitWorkingPanel();
begin
  WorkingPanel := NewPanel('working');
  workingPanel^.area.x := 175;
  workingPanel^.area.y := 25;
  workingPanel^.draggable := true;
  ShowPanel(workingPanel);
end;

procedure Main();
var
	lst: GUIList;
	
begin
  OpenAudio();
  OpenGraphicsWindow('Interface Editor', 800, 600);
  
  //ToggleFullScreen();
  
  LoadResourceBundle('InterfaceEditorBundle.txt');
  
  InitInterface();
  InitWorkingPanel();
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    
    MyUpdateGUI();
    
    
    UpdateInterface();
    DrawInterface();
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.