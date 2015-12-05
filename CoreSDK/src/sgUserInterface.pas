//=============================================================================
// sgUserInterface.pas
//=============================================================================
//
// Resposible for constructing User Interfaces in SwinGame projects.. eventually.
//
//=============================================================================


/// The User Interface code is used to create Graphical User Interface components in SwinGame
///
///@module UserInterface
///@static
unit sgUserInterface;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

  
  
//---------------------------------------------------------------------------
// Alter GUI global values
//---------------------------------------------------------------------------
  ///Sets the ForeGround color of the GUI
  ///
  /// @lib
  ///
  procedure GUISetForegroundColor(c:Color);
  
  ///Sets the Background color of the GUI
  ///
  /// @lib
  ///
  procedure GUISetBackgroundColor(c:Color);
  
  ///Sets the inactive ForeGround color of the GUI
  ///
  /// @lib
  ///
  procedure GUISetBackgroundColorInactive(c:color);
  
  ///Sets the inactive ForeGround color of the GUI
  ///
  /// @lib
  ///
  procedure GUISetForegroundColorInactive(c:color);
  
  /// Returns true if any of the panels in the user interface have been clicked.
  /// 
  /// @lib
  function GUIClicked(): Boolean;
  
  
//---------------------------------------------------------------------------
// Panels
//---------------------------------------------------------------------------
  
  
  ///Creates a new panel
  ///
  /// @lib
  /// @sn createPanelNamed:%s
  ///
  /// @class Panel
  /// @constructor
  function NewPanel(const pnlName: String): Panel;
  
  /// shows dialog panel
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method ShowDialog
  procedure ShowPanelDialog(p: Panel);
  
  /// Display the panel on screen at panel's co-ordinates.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method Show
  procedure ShowPanel(p: Panel);
  
  /// Display the panel on screen at panel's co-ordinates.
  ///
  /// @lib ShowPanelNamed
  procedure ShowPanel(const name: String);
  
  /// Hide the panel, stop drawing it. Panels which are not being draw can not be interacted with by the user.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method Hide
  procedure HidePanel(p: Panel);
  
  /// Hide the panel, stop drawing it. Panels which are not being draw can not be interacted with by the user.
  ///
  /// @lib HidePanelNamed
  procedure HidePanel(const name: String);
  
  /// Toggles whether the panel is being shown or not.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method ToggleVisible
  procedure ToggleShowPanel(p: Panel);
  
  /// Draw the currently visible panels (For use in the main loop)
  ///
  /// @lib
  procedure DrawInterface();
  
  /// Activate the passed in panel. If shown, the panel will be clickable. This is the default state of a panel.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method Activate
  procedure ActivatePanel(p: Panel);
  
  /// Deactivate the panel. The panel will become unclickable, it will remain visible if it was already.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method Deactivate
  procedure DeactivatePanel(p: Panel);
  
  /// Activates the panel if deactivated, and deactivates if activated.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @method ToggleActivate
  procedure ToggleActivatePanel(p: Panel);
  
  /// Returns whether panel is active
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter Active
  function PanelActive(pnl: panel): Boolean;
  
  /// Returns true if panel is currently visible.
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter Visible
  function PanelVisible(p: Panel): Boolean;
  
  /// Returns the last panel clicked.
  ///
  /// @lib
  function PanelClicked(): Panel; overload;
  
  /// Returns true when the panel was clicked.
  ///
  /// @lib PanelWasClicked
  ///
  /// @class Panel
  /// @getter Clicked
  function PanelClicked(pnl: Panel): Boolean; overload;
  
  /// Sets panel's draggability to the passed Boolean
  ///
  /// @lib
  /// @sn panel:%s setDraggable:%s
  ///
  /// @class Panel
  /// @setter Draggable
  procedure PanelSetDraggable(p: panel; b:Boolean);
  
  /// Returns whether or not the passed panel is currently draggable
  ///
  /// @lib
  /// @sn panelDraggable:%s
  ///
  /// @class Panel
  /// @getter Draggable
  function PanelDraggable(p: panel): Boolean;
  
  /// Move panel along vector
  ///
  /// @lib
  /// @sn panel:%s moveBy:%s
  ///
  /// @class Panel
  /// @method Move
  procedure MovePanel(p: Panel; const mvmt: Vector);
  
  /// Returns the panel at the point passed in. Returns nil if there is no panel.
  ///
  /// @lib
  function PanelAtPoint(const pt: Point2D): Panel;
  
  /// Returns true if point is in a region with the indicate `kind` of the panel `p`.
  ///
  /// @lib PointInRegionWithKind
  /// @sn pointInRegion:%s ofPanel:%s kind:%s
  ///
  /// @class Panel
  /// @self 2
  /// @overload PointInRegion PointInRegionWithKind
  /// @csn point:%s inRegionKind:%s
  function PointInRegion(const pt: Point2D; p: Panel; kind: GUIElementKind): Boolean; overload;
  
  /// Returns true if point is in any region within the panel
  ///
  /// @lib
  /// @sn pointInRegion:%s ofPanel:%s
  ///
  /// @class Panel
  /// @self 2
  /// @method PointInRegion
  function PointInRegion(const pt: Point2D; p: Panel): Boolean; overload;
  
  /// Disposes of all panels
  ///
  /// @lib
  procedure ReleaseAllPanels();
  
  /// Disposes of the panel by name, removing it from the index collection, setting its dragging to nil, and hiding it first to avoid crashes.
  ///
  /// @lib
  procedure ReleasePanel(const name: String);
  
  /// Disposes of the panel by panel
  ///
  /// @lib
  ///
  /// @class Panel
  /// @dispose
  procedure FreePanel(var pnl: Panel);
  
  /// maps panel to name in Hash Table.
  ///
  /// @lib
  /// @sn loadPanelNamed:%s fromFile:%s
  ///
  /// @class Panel
  /// @constructor
  /// @csn initWithName:%s fromFile:%s
  function LoadPanelNamed(const name, filename: String): Panel;
  
  /// Loads panel from panel directory with filename
  ///
  /// @lib
  ///
  /// @class Panel
  /// @constructor
  /// @csn initFromFile:%s
  function LoadPanel(const filename: String): Panel;
  
  /// Returns if panel is in Index Collection
  ///
  /// @lib
  function HasPanel(const name: String): Boolean;
  
  /// Returns panel with the name name
  ///
  /// @lib
  function PanelNamed(const name: String): Panel;  
  
  /// Returns the name of the panel
  ///
  /// @lib
  /// 
  /// @class Panel
  /// @getter Name
  function PanelName(pnl: Panel): String;  
  
  /// Returns panel filename
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter FileName
  function PanelFilename(pnl: Panel): String;
  
  /// Returns if anything is currently being dragged
  ///
  /// @lib
  function IsDragging(): Boolean; overload;
  
  /// Returns if panel is currently being dragged
  ///
  /// @lib PanelIsDragging
  ///
  /// @class Panel
  /// @getter IsDragging
  function IsDragging(pnl: Panel): Boolean; overload;
  
  /// Returns panel y value
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter Y
  function PanelY(p: Panel): Single;
  
  /// Returns panel x value
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter X
  function PanelX(p: Panel): Single;
  
  /// Returns panel h value
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter Height
  function PanelHeight(p: Panel): Longint;
  
  /// Returns height of the panel
  ///
  /// @lib PanelNamedHeight
  function PanelHeight(const name: String): Longint;

  /// Returns the panel's width
  ///
  /// @lib
  ///
  /// @class Panel
  /// @getter Width
  function PanelWidth(p: Panel): Longint;
  
  /// Returns the panel's width
  ///
  /// @lib PanelNamedWidth
  function PanelWidth(const name: String): Longint;



// ===========
// = Buttons =
// ===========

  /// Returns true when the region has been clicked.
  ///
  /// @lib
  function ButtonClicked(r: Region) : Boolean;
  
  /// Returns true when the region has been clicked.
  ///
  /// @lib ButtonNamedClicked
  function ButtonClicked(const name: String) : Boolean;
  
  
  
//---------------------------------------------------------------------------
// Regions
//---------------------------------------------------------------------------
  
  /// Returns the ID of the last region clicked on by user.
  ///
  /// @lib
  function RegionClickedID(): String;

  /// Returns the last region clicked on by user.
  ///
  /// @lib
  function RegionClicked(): Region;

  /// Returns the ID of the last region clicked on by user.
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter ID
  function RegionID(r: Region): String;

  /// Returns the Region with the ID passed from the panel passed
  ///
  /// @lib
  /// @sn regionOfPanel:%s withId:%s
  ///
  /// @class Panel
  /// @method RegionWithID
  function RegionWithID(pnl: Panel; const ID: String): Region; overload;
  
  /// Returns the Region with the ID passed
  ///
  /// @lib GlobalRegionWithID
  function RegionWithID(const ID: String): Region; overload;

  /// Returns the Region with the ID passed from the panel passed
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter ParentPanel
  function RegionPanel(r: Region): Panel;

  /// Toggles the region active state
  ///
  /// @lib
  ///
  /// @class Region
  /// @method ToggleActive
  procedure ToggleRegionActive(forRegion: Region);
  
  /// Returns true when the region is active.
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter RegionActive
  function RegionActive(forRegion: Region): Boolean;
  
  /// Sets the region active to Boolean
  ///
  /// @lib
  /// @sn region:%s setActive:%s
  ///
  /// @class Region
  /// @setter RegionActive
  procedure SetRegionActive(forRegion: Region; b: Boolean);

  /// Returns the Region Y value
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter Y
  function RegionY(r: Region): Single;

  /// Returns the Region X value
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter X
  function RegionX(r: Region): Single;

  /// Returns the Region height value
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter Height
  function RegionHeight(r: Region): Longint;

  /// Returns the Region Wdith value
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter Width
  function RegionWidth(r: Region): Longint;

  /// Returns the font used for text rendered in this region.
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter Font
  function RegionFont(r: Region): Font;

  /// Sets the font for the region
  /// 
  /// @lib 
  /// @sn region:%s setFont:%s
  ///
  /// @class Region
  /// @setter Font
  procedure RegionSetFont(r: REgion; f: Font);

  /// Returns the font alignment of text for this region.
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter FontAlignment
  function RegionFontAlignment(r: Region): FontAlignment;

  /// Allows the font to be set for a region
  ///
  /// @lib
  /// @sn region:%s setFontAlignment:%s
  ///
  /// @class Region
  /// @setter FontAlignment
  procedure RegionSetFontAlignment(r: Region; align: FontAlignment);



  /// Returns the region from the panel at the point
  ///
  /// @lib
  /// @sn regionOnPanel:%s atPoint:%s
  function RegionAtPoint(p: Panel; const pt: Point2D): Region;

  /// Registers the callback with the panel, when an event related to this
  /// region occurs the procedure registered will be called.
  ///
  /// @lib
  /// @sn region:%s registerEventCallback:%s
  /// 
  /// @class Region
  /// @method  RegisterEventCallback
  procedure RegisterEventCallback(r: Region; callback: GUIEventCallback);



//---------------------------------------------------------------------------
//Checkbox
//---------------------------------------------------------------------------

/// Returns checkbox state of the checkbox with ID from string
///
/// @lib
function CheckboxState(const s: String): Boolean; overload;

/// Returns checkbox state of the checkbox with ID in a given Panel
///
/// @lib CheckboxStateOnPanel/// @sn panel:%s checkboxStateForId:%s
///
/// @class Panel
/// @method CheckboxState
/// @csn checkboxStateWithId:%s
function CheckboxState(p: Panel; const s: String): Boolean; overload;

/// Returns checkbox state of the checkbox with ID from string
///
/// @lib CheckboxStateFromRegion
///
/// @class Region
/// @getter CheckboxState
function CheckboxState(r: Region): Boolean; overload;

/// Sets the checkbox state to val.
/// 
/// @lib CheckboxSetStateFromRegion
/// @sn region:%s setCheckBotState:%s
///
/// @class Region
/// @setter CheckboxState
procedure CheckboxSetState(r: region; val: Boolean); overload;

/// Sets the checkbox state to val given the ID.
/// 
/// @lib CheckBoxSetStateWithId
/// @sn checkbox:%s setState:%s
procedure CheckboxSetState(const id: String ; val: Boolean); overload;


/// Sets the checkbox state to val.
/// 
/// @lib CheckboxSetStateOnPanel/// @sn checkboxOnPanel:%s withID:%s setState:%s
procedure CheckboxSetState( pnl: Panel; const id: String; val: Boolean); overload;
  
/// Toggles the state of a checkbox (ticked/unticked)
///
/// @lib ToggleCheckboxStateOnPanel/// @sn toggleCheckBoxOnPanel:%s withID:%s
///
/// @class Panel
/// @method ToggleCheckboxStateWithID
/// @csn toggleCheckBoxWithID:%s
procedure ToggleCheckboxState(pnl : Panel; const id: String);

  
/// Toggles the state of a checkbox (ticked/unticked)
///
/// @lib ToggleCheckboxStateFromID
procedure ToggleCheckboxState(const id: String);


// ===============
// = Radio Group =
// ===============
  
/// Takes a radiogroup and returns the active button's index.
///
/// @lib ActiveRadioButtonIndexFromID
function ActiveRadioButtonIndex(const id: String): LongInt;

/// Takes a radiogroup and returns the active button's index.
///
/// @lib ActiveRadioButtonIndexOnPanel/// @sn activeRadioButtonIndexOnPanel:%s withId:%s
///
/// @class Panel
/// @method ActiveRadioButtonIndexWithID
function ActiveRadioButtonIndex(pnl: Panel; const id: String): LongInt;

/// Takes an ID and returns the active button
///
/// @lib ActiveRadioButtonWithID
function ActiveRadioButton(const id: String): Region; overload;

/// Takes a panel and an ID and returns the active button
///
/// @lib ActiveRadioButtonOnPanelWithId
/// @sn activeRadioButtonOnPanel:%s withId:%s
///
/// @class RadioGroup
/// @getter ActiveButton
function ActiveRadioButton(pnl: Panel; const id: String): Region; overload;

/// Takes a region and an ID and selects the button
///
/// @lib SelectRadioButton
procedure SelectRadioButton(r: Region); overload;

/// Takes an ID and returns the active button
///
/// @lib SelectRadioButtonWithID
procedure SelectRadioButton(const id: String); overload;

/// Takes a panel and an ID and selects the button
///
/// @lib SelectRadioButtonOnPanelWithId
/// @sn radioButtonOnPanel:%s withId:%s
///
/// @class Panel
/// @method SelectRadioButton
procedure SelectRadioButton(pnl : Panel; const id: String); overload;

// ===========
// = Textbox =
// ===========

/// Gets the textbox text from region
/// 
/// @lib TextboxTextWithId
function TextBoxText(const id: String): String; overload;


/// Gets the textbox text from region
/// 
/// @lib TextboxTextOnPanelWithId
/// @sn textboxTextOnPanel:%s withId:%s
function TextBoxText(pnl : Panel; const id: String): String; overload;

/// Gets the textbox text from region
/// 
/// @lib TextboxTextFromRegion
///
/// @class Region
/// @getter TextboxText
function TextBoxText(r: Region): String; overload;

/// Sets the textbox text from region
/// 
/// @lib TextboxSetTextFromRegion
/// @sn textboxForRegion:%s setText:%s
///
/// @class Region
/// @setter TextboxText
procedure TextboxSetText(r: Region; const s: string); overload;

/// Sets the textbox text from Id
/// 
/// @lib TextboxSetTextFromId
/// @sn textboxWithId:%s setText:%s
procedure TextboxSetText(const id, s: string); overload;

/// Sets the textbox text from Panel and Id
/// 
/// @lib TextboxSetTextOnPanelAndId
/// @sn textboxOnPanel:%s withId:%s setText:%s
procedure TextboxSetText(pnl: Panel; const id, s: string); overload;

/// Sets the textbox text from region
/// 
/// @lib TextboxSetTextToIntFromRegion
/// @sn textboxForRegion:%s setInt:%s
procedure TextboxSetText(r: Region; i: Longint); overload;  
  
/// Sets the textbox text from Id
/// 
/// @lib TextboxSetTextToIntWithId
/// @sn textboxForId:%s setInt:%s
procedure TextboxSetText(const id : String; i : LongInt); overload;

/// Sets the textbox text from panel and Id
/// 
/// @lib TextboxSetTextToIntOnPanelWithId
/// @sn textboxOnPanel:%s withId:%s setInt:%s
procedure TextboxSetText(pnl : Panel; const id : String; i : LongInt); overload;
      
/// Sets the textbox text from region
/// 
/// @lib TextboxSetTextToSingleFromRegion
/// @sn textboxForRegion:%s setFloat:%s
procedure TextboxSetText(r: Region; single: Single); overload;
  
/// Sets the textbox text from region
/// 
/// @lib TextboxSetTextToSingleFromId
/// @sn textboxWithId:%s setFloat:%s
procedure TextboxSetText(const id : String; single: Single); overload;
                        
/// Sets the textbox text from Panel and Id
/// 
/// @lib TextboxSetTextToSingleOnPanel
/// @sn textboxOnPanel:%s withId:%s setFloat:%s
procedure TextboxSetText(pnl : Panel; const id : String; single: Single); overload;

/// Sets the active textbox from region
/// 
/// @lib GUISetActiveTextboxFromRegion
procedure GUISetActiveTextbox(r: Region); overload;

/// Sets the active textbox to the one with the
/// indicated name.
/// 
/// @lib GUISetActiveTextboxNamed
procedure GUISetActiveTextbox(const name: String); overload;

/// Deactivates the active textbox
///
/// @lib
procedure DeactivateTextBox();

/// Returns the index of the active textbox's region.
///
/// @lib
function ActiveTextIndex(): Longint;

/// Checks if TextEntry finished, returns true/false
///
/// @lib
function GUITextEntryComplete(): Boolean;

/// Returns the region of the textbox in which text was changed/added into most recently.
///
/// @lib
function RegionOfLastUpdatedTextBox(): Region;

/// Returns the index of the region of the textbox in which text was changed/added into most recently.
///
/// @lib
function IndexOfLastUpdatedTextBox(): Longint;

/// UpdateInterface main loop, checks the draggable, checks the region clicked, updates the interface
///
/// @lib
procedure UpdateInterface();

/// Sets the GUI whether or not to use Vector Drawing
///
/// @lib
procedure DrawGUIAsVectors(b : Boolean);

/// Finishes reading text and stores in the active textbox
///
/// @lib
procedure FinishReadingText();

/// Returns the parent panel of the active textbox
///
/// @lib
function ActiveTextBoxParent() : Panel;

//---------------------------------------------------------------------------
//Lists
//---------------------------------------------------------------------------

/// Removes the active item from a list
/// 
/// @lib ListRemoveActiveItemFromRegion
///
/// @class Region
/// @method ListRemoveActiveItem
procedure ListRemoveActiveItem(r : region); overload;

/// Removes the active item from a list
/// 
/// @lib ListRemoveActiveItemFromId
procedure ListRemoveActiveItem(const id : string); overload;

/// Removes the active item from a list
/// 
/// @lib ListRemoveActiveItemOnPanelWithId
/// @sn listRemoveActiveItemOnPanel:%s withId:%s
procedure ListRemoveActiveItem(pnl : Panel; const id: string); overload;

/// Set the active item in the list to the item at index idx
///
/// @lib ListSetActiveItemIndexWithId
/// @sn listSetActiveItemWithId:%s atIndex:%s
procedure ListSetActiveItemIndex(const id : String; idx: Longint);

/// Set the active item in the list to the item at index idx
///
/// @lib ListSet
/// @sn listSetActiveItemFromPane:%s withId:%s at:%s
procedure ListSetActiveItemIndex(pnl : Panel; const id : String; idx: Longint);

/// Returns the text of the item at index idx from the List of the Region
///
/// @lib ListItemTextFromRegion
/// @sn listForRegion:%s textAtIndex:%s
///
/// @class Region
/// @method ListItemTextAtIndex
function ListItemText(r: Region; idx: Longint): String; overload;

/// Returns the text of the item at index idx
///
/// @lib ListItemTextFromId
/// @sn listWithId:%s textAtIndex:%s
function ListItemText(const id : String; idx: Longint): String; overload;

/// Returns the text of the item at index idx
///
/// @lib ListItemTextOnPanelWithId
/// @sn listOnPanel:%s withId :%s textAtIndex:%s
function ListItemText(pnl : Panel; const id : String; idx: Longint): String; overload;

/// Returns the text of the active item in the list of the region
///
/// @lib ListActiveItemTextFromRegion
///
/// @class Region
/// @getter ListItemText
function ListActiveItemText(r: Region): String; overload;

/// Returns the active item text of the List in panel, pnl- with ID, ID
///
/// @lib ListActiveItemTextOnPanelWithId
/// @sn listActiveItemTextOnPanel:%s withId:%s
function ListActiveItemText(pnl: Panel; const ID: String): String; overload;

/// Returns the active item text of the List in with ID
///
/// @lib ListWithIdActiveItemText
function ListActiveItemText(const ID: String): String; overload;
                            
/// Returns the number of items in the list of the region
///
/// @lib ListItemCountFromRegion
///
/// @class Region
/// @getter ListItemCount
function ListItemCount(r: Region): Longint; overload;

/// Returns the number of items in the list
///
/// @lib ListItemCountWithId
function ListItemCount(const id : String): Longint; overload;

/// Returns the number of items in the list
///
/// @lib ListItemCountOnPanelWithId
/// @sn listItemCountFrom:%s withId:%s
function ListItemCount(pnl : Panel; const id : String): Longint; overload;

/// Returns active item's index from the list of the region
///
/// @lib ListActiveItemIndexFromRegion
///
/// @class Region
/// @getter ListActiveItemIndex
function ListActiveItemIndex(r: Region): Longint; overload;

/// Returns active item's index from the list
///
/// @lib ListActiveItemIndexWithId
///
function ListActiveItemIndex(const id: String): Longint; overload;

/// Returns active item's index from the list
///
/// @lib ListActiveItemIndexOnPanelWithId
/// @sn listActiveItemIndexOnPanel:%s withId:%s
function ListActiveItemIndex(pnl : Panel; const id: String): Longint; overload;

/// Removes item at index idx from the list
///
/// @lib ListRemoveItemOnPanelWithId
/// @sn listOnPanel:%s withId:%s removeItemAtIndex:%s
procedure ListRemoveItem(pnl : Panel; const id : String; idx: Longint);

/// Removes item at index idx from the list
///
/// @lib ListRemoveItemFromWithId
/// @sn listWithId:%s removeItemAtIndex:%s
procedure ListRemoveItem(const id : String; idx: Longint);

/// Removes all items from the list.
///
/// @lib ListclearItemsWithId
///
procedure ListClearItems(const id : String); overload;

/// Removes all items from the list.
///
/// @lib ListClearItemsGivenPanelWithId
/// @sn clearItemsListOnPanel:%s withId:%s
procedure ListClearItems(pnl : Panel; const id : String); overload;

/// Removes all items from the list of the region
///
/// @lib ListClearItemsFromRegion
///
/// @class Region
/// @method ListClearItems
procedure ListClearItems(r : Region); overload;  

/// Adds an item to the list by text
///
/// @lib AddItemWithIdByText
/// @sn listWithId:%s addText:%s
procedure ListAddItem(const id, text: String); overload;


/// Adds an item to the list by text
///
/// @lib AddItemOnPanelWithIdByText
/// @sn listOnPanel:%s withId:%s addText:%s
procedure ListAddItem(pnl : Panel; const id, text: String); overload;
  
/// Adds an item to the list by bitmap
///
/// @lib AddItemWithIdByBitmap
/// @sn listWithId:%s addBitmap:%s
procedure ListAddItem(const id : String; img:Bitmap); overload;
  
/// Adds an item to the list by bitmap
///
/// @lib ListAddItemBitmap
/// @sn listOnPanel:%s withId:%s addBitmap:%s
procedure ListAddItem(pnl : Panel; const id : String; img:Bitmap); overload;

/// Adds an item to the list by text and Bitmap
///
/// @lib ListWithIDAddBitmapWithTextItem
/// @sn listById:%s addBitmap:%s andText:%s
procedure ListAddItem(const id : String; img: Bitmap; const text: String); overload;

/// Adds an item to the list by text and Bitmap
///
/// @lib ListOnPanelWithIdAddBitmapWithTextItem
/// @sn listOnPanel:%s withId:%s addBitmap:%s andText:%s
procedure ListAddItem(pnl : Panel; const id : String; img: Bitmap; const text: String); overload;

/// Adds an item to the list where the items shows a cell of a
/// bitmap.
///
/// @lib ListWithIdAddItemWithCell
/// @sn listWithId:%s addBitmap:%s cell:%s
///
procedure ListAddItem(const id : String; img: Bitmap; cell: Longint); overload;
  
/// Adds an item to the list where the items shows a cell of a
/// bitmap.
///
/// @lib ListOnPanelWithIdAddItemWithCell
/// @sn listOnPanel:%s withId:%s addBitmap:%s cell:%s
///
procedure ListAddItem(pnl : Panel; const id : String; img: Bitmap; cell: Longint); overload;

/// Adds an item to the list where the items shows a cell of a
/// bitmap and some text.
///
/// @lib ListWithIdAddItemWithCellAndText
/// @sn listWithId:%s addBitmap:%s cell:%s andText:%s
///
procedure ListAddItem(const id : String; img: Bitmap; cell: Longint; const text: String); overload;

/// Adds an item to the list where the items shows a cell of a
/// bitmap and some text.
///
/// @lib ListOnPanelWithIdAddItemWithCellAndText
/// @sn listOnPanel:%s withId:%s addBitmap:%s cell:%s andText:%s
///
procedure ListAddItem(pnl : Panel; const id : String; img: Bitmap; cell: Longint; const text: String); overload;

/// Adds an item to the list where the items shows a cell of a
/// bitmap and some text.
///
/// @lib ListAddItemWithCellAndTextFromRegion
/// @sn region:%s addBitmap:%s cell:%s andText:%s
///
/// @class Region
/// @overload AddItem AddItemWithCellAndTextFromRegion
/// @csn addBitmap:%s cell:%s andText:%s
procedure ListAddItem(r: Region; img: Bitmap; cell: Longint; const text: String); overload;


/// Adds an item to the list where the items shows a cell of a
/// bitmap.
///
/// @lib ListAddItemWithCellFromRegion
/// @sn region:%s addBitmap:%s cell:%s
///
/// @class Region
/// @overload AddItem AddItemWithCellFromRegion
/// @csn addBitmap:%s cell:%s

procedure ListAddItem(r : Region; img: Bitmap; cell: Longint); overload;

/// Adds an item to the list by text
///
/// @lib ListAddItemByTextFromRegion
/// @sn listOfRegion:%s addText:%s
///
/// @class Region
/// @method ListAddTextItem
procedure ListAddItem(r : Region; const text: String); overload;


/// Adds an item to the list by bitmap
///
/// @lib ListAddItemByBitmapFromRegion
/// @sn listForRegion:%s addBitmap:%s
///
/// @class Region
/// @method ListAddBitmapItem
procedure ListAddItem(r : Region; img:Bitmap); overload;

/// Adds an item to the list
///
/// @lib ListAddBitmapAndTextItemFromRegion
/// @sn  listForRegion:%s addBitmap:%s andText:%s
///
/// @class Region
/// @method ListAddBitmapAndTextItem
/// @csn listAddBitmap:%s andText:%s
procedure ListAddItem(r : Region; img:Bitmap; const text: String); overload;

/// Returns the starting point for the list from region
///
/// @lib ListStartingAtFromRegion
///
/// @class Region
/// @getter ListStartAt
function ListStartAt(r: Region): Longint;

/// Sets the starting point for the list from region
///
/// @lib ListSetStartingAtFromRegion
/// @sn listForRegion:%s setStartat:%s
///
/// @class Region
/// @setter ListStartAt
procedure ListSetStartAt(r: Region; idx: Longint);

// =========
// = Label =
// =========

/// Get text From Label
///
/// @lib LabelTextFromRegion
function  LabelText(r: Region): string; overload;

/// Get text From Label
///
/// @lib LabelTextWithId
function  LabelText(const id : String): string; overload;

/// Get text From Label
///
/// @lib LabelTextOnPanelWithId
/// @sn textOfLabelOnPanel:%s withId:%s
function  LabelText(pnl : Panel; const id : String): string; overload;

/// Set text for Label
///
/// @lib LabelWithIdSetText
/// @sn labelWithId:%s setText:%s
procedure LabelSetText(const id, newString: String); overload;

/// Set text for Label
///
/// @lib LabelFromRegionSetText
/// @sn labelForRegion:%s setText:%s
procedure LabelSetText(r: Region; const newString: String); overload;

/// Set text for Label
///
/// @lib LabelOnPanelWithIdSetText
/// @sn labelOnPanel:%s withId:%s setText:%s
procedure LabelSetText(pnl: Panel; const id, newString: String); overload;

// ===============
// = Dialog Code =
// ===============

/// Sets the path of the dialog
///
/// @lib 
/// @class Dialog
/// @method setPath
procedure DialogSetPath(const fullname: String);
  
/// Gets the path of the dialog
///
/// @lib 
function DialogPath(): String;

/// Gets if the dialog has been cancelled
///
/// @lib 
function DialogCancelled(): Boolean;

/// Gets if the dialog has been Completed
///
/// @lib 
function DialogComplete(): Boolean;

/// Displays a SaveDialog
///
/// @lib 
procedure ShowSaveDialog(); overload;

/// Displays a SaveDialog with file/folder/both filter
///
/// @lib ShowSaveDialogWithType
procedure ShowSaveDialog(select: FileDialogSelectType); overload;
/// Displays an OpenDialog
///
/// @lib 
procedure ShowOpenDialog(); overload;
  
/// Displays an OpenDialog file/folder/both filter
///
/// @lib ShowOpenDialogWithType
procedure ShowOpenDialog(select: FileDialogSelectType); overload;


//Do not use
//procedure AddPanelToGUI(p: Panel);
//procedure AddRegionToPanelWithString(const d: string; p: panel);

//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, Math,
    stringhash, sgSharedUtils, sgNamedIndexCollection,   // libsrc
    sgShared, sgResources, sgTrace, sgImages, sgGraphics,
    sgGeometry, sgText, sgInput, sgAudio, sgDrawingOptions,
    sgBackendTypes;
//=============================================================================

procedure DoFreePanel(var pnl: Panel); forward;

type
  GUIController = record
    panels:                Array of PanelPtr;         // The panels that are loaded into the GUI
    visiblePanels:         Array of PanelPtr;         // The panels that are currently visible (in reverse z order - back to front)
    globalGUIFont:         Font;
    foregroundClr:         Color;                  // The color of the foreground
    backgroundClr:         Color;                  // The color of the background
    foregroundClrInactive: Color;
    backgroundClrInactive: Color;
    VectorDrawing:         Boolean;
    lastClicked:           RegionPtr;
    activeTextBox:         RegionPtr;
    lastActiveTextBox:     RegionPtr;
    doneReading:           Boolean;
    lastTextRead:          String;                 // The text that was in the most recently changed textbox before it was changed.
    panelIds:              TStringHash;
    
    // Variables for dragging panels
    downRegistered:     Boolean;
    panelDragging:      PanelPtr;
    
    // The panel clicked
    panelClicked:       PanelPtr;
  end;

  FileDialogData = packed record
    dialogPanel:          PanelPtr;      // The panel used to show the dialog
    currentPath:          String;     // The path to the current directory being shown
    currentSelectedPath:  String;     // The path to the file selected by the user
    cancelled:            Boolean;
    complete:             Boolean;
    allowNew:             Boolean;
    selectType:           FileDialogSelectType;
    onlyFiles:            Boolean;
    lastSelectedFile, lastSelectedPath: Longint; // These represent the index if the selected file/path to enable double-like clicking
  end;
  
var
  GUIC: GUIController;
  // The file dialog
  dialog: FileDialogData;

function RegionPtrAtPoint(p: PanelPtr; const pt: Point2D): RegionPtr; forward;


/// Returns checkbox state of the given checkbox
///
/// @lib CheckboxStateFromCheckbox
/// 
/// @class Checkbox
/// @getter State
function CheckboxState(chk: GUICheckbox): Boolean; overload; forward;

/// Sets the checkbox state to val.
/// 
/// @lib
/// @sn checkBox:%s setCheckBoxState:%s
///
/// @class Checkbox
/// @setter State
procedure CheckboxSetState(chk: GUICheckbox; val: Boolean); overload; forward;

/// Toggles the state of a checkbox (ticked/unticked)
///
/// @lib
///
/// @class Checkbox
/// @method ToggleState
procedure ToggleCheckboxState(c: GUICheckbox); forward;

/// Takes a region and returns the checkbox of that region
///
/// @lib
function CheckboxFromRegion(r: RegionPtr): GUICheckbox; forward;

  /// Takes panel and ID and returns the RadioGroup.
  ///
  /// @lib RadioGroupOnPanelWidthId
  /// @sn radioGroupOnPanel:%s withId:%s
  ///
  /// @class GUIRadioGroup
  /// @constructor
  /// @csn initOnPanel:%s withId :%s
  function RadioGroupFromId(pnl: PanelPtr; const id: String): GUIRadioGroup;overload; forward;

  /// Takes an ID and returns the RadioGroup.
  ///
  /// @lib
  function RadioGroupFromId(const id: String): GUIRadioGroup;overload; forward;

  /// Takes region and returns the RadioGroup.
  ///
  /// @lib
  function RadioGroupFromRegion(r: RegionPtr): GUIRadioGroup; forward;

  /// Takes a radiogroup and returns the active button's index.
  ///
  /// @lib
  ///
  /// @class RadioGroup
  /// @getter ActiveButtonIndex
  function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): LongInt; forward;

  /// Takes a radiogroup and returns the active button
  ///
  /// @lib ActiveRadioButton
  ///
  /// @class RadioGroup
  /// @getter ActiveButton
  function ActiveRadioButton(grp: GUIRadioGroup): Region; overload; forward;

  /// Takes a RadioGroup and Region and selects the button
  ///
  /// @lib SelectRadioButtonFromRadioGroupAndRegion
  /// @sn radioGroup:%s selectButton:%s
  ///
  /// @class RadioGroup
  /// @method SelectRadioButton
  procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload; forward;

  /// Takes a RadioGroup and index and selects the button
  ///
  /// @lib SelectRadioButtonFromRadioGroupAndIndex
  /// @sn radioGroup:%s selectButtonIdx:%s
  ///
  /// @class RadioGroup
  /// @method SelectRadioButton
  procedure SelectRadioButton(rGroup: GUIRadioGroup; idx: LongInt); overload; forward;

  /// Gets the textbox text
  /// 
  /// @lib
  ///
  /// @class Textbox
  /// @getter Text
  function TextBoxText(tb: GUITextBox): String; overload; forward;

  /// Sets the textbox text from region
  /// 
  /// @lib TextboxSetText
  /// @sn textbox:%s setText:%s
  ///
  /// @class Textbox
  /// @setter Text
  procedure TextboxSetText(tb: GUITextBox; const s: string); overload; forward;

  /// Sets the textbox text from region
  /// 
  /// @lib TextboxSetTextToInt
  /// @sn textbox:%s setInt:%s
  procedure TextboxSetText(tb: GUITextBox; i: Longint); overload; forward;

  /// Sets the textbox text from Textbox
  /// 
  /// @lib TextboxSetTextToSingle
  /// @sn textbox:%s setFloat:%s
  procedure TextboxSetText(tb: GUITextBox; single: Single); overload; forward;

  /// Sets the textbox text from region
  /// 
  /// @lib TextboxFromRegion
  function TextBoxFromRegion(r: RegionPtr): GUITextBox; forward;

  /// Sets the active textbox
  /// 
  /// @lib
  procedure GUISetActiveTextbox(t: GUITextBox); overload; forward;

  /// Returns the textbox in which text was changed/added into most recently.
  ///
  /// @lib
  function GUITextBoxOfTextEntered(): GUITextbox; forward;

  /// The the TextBox from an ID
  ///
  /// @lib
  function TextBoxFromID(const id : String) : GUITextbox;  forward;

  /// Set the active item in the list to the item at index idx
  ///
  /// @lib
  /// @sn list:%s setActiveItemIndex:%s
  ///
  /// @class GUIList
  /// @setter ActiveItemIndex
  procedure ListSetActiveItemIndex(lst: GUIList; idx: Longint); forward;

  /// Returns Returns the list of the region r
  ///
  /// @lib
  ///
  /// @class Region
  /// @getter List
  function ListFromRegion(r: RegionPtr): GUIList; overload;  forward;

  /// Returns the text of the item at index idx
  ///
  /// @lib
  /// @sn list:%s textAtIndex:%s
  ///
  /// @class GUIList
  /// @method ItemTextAtIndex
  function ListItemText(lst: GUIList; idx: Longint): String; overload; forward;

  /// Returns the text of the active item in the list of the region
  ///
  /// @lib
  ///
  /// @class GUIList
  /// @getter ActiveItemText
  function ListActiveItemText(list: GUIList): String; overload; forward;

  /// Returns the number of items in the list
  ///
  /// @lib ListItemCount
  ///
  /// @class GUIList
  /// @getter ItemCount
  function ListItemCount(lst:GUIList): Longint; overload; forward;

  /// Returns active item's index from the list
  ///
  /// @lib
  ///
  /// @class GUIList
  /// @getter ActiveItemIndex
  function ListActiveItemIndex(lst: GUIList): Longint; overload; forward;

  /// Removes item at index idx from the list
  ///
  /// @lib
  /// @sn list:%s removeItemAtIndex:%s
  ///
  /// @class GUIList
  /// @method RemoveItem
  procedure ListRemoveItem(lst: GUIList; idx: Longint); forward;

  /// Removes all items from the list.
  ///
  /// @lib
  ///
  /// @class GUIList
  /// @method ClearItems
  procedure ListClearItems(lst: GUIList); overload; forward;

  /// Adds an item to the list by text
  ///
  /// @lib AddItemByText
  /// @sn list:%s addText:%s
  ///
  /// @class GUIList
  /// @method AddTextItem
  procedure ListAddItem(lst: GUIList; const text: String); overload; forward;

  /// Adds an item to the list by bitmap
  ///
  /// @lib AddItemByBitmap
  /// @sn list:%s addBitmap:%s
  ///
  /// @class GUIList
  /// @method AddBitmapItem
  procedure ListAddItem(lst: GUIList; img:Bitmap); overload; forward;

  /// Adds an item to the list by text and Bitmap
  ///
  /// @lib ListAddBitmapAndTextItem
  /// @sn list:%s addItemBitmap:%s withText:%s
  ///
  /// @class GUIList
  /// @method AddBitmapWithTextItem
  /// @csn addImg:%s andText:%s
  procedure ListAddItem(lst: GUIList; img: Bitmap; const text: String); overload; forward;

  /// Adds an item to the list where the items shows a cell of a
  /// bitmap.
  ///
  /// @lib ListAddItemWithCell
  /// @sn list:%s addBitmap:%s cell:%s
  ///
  /// @class GUIList
  /// @overload AddItem AddItemWithCell
  /// @csn addBitmap:%s cell:%s
  procedure ListAddItem(lst: GUIList; img: Bitmap; cell: Longint); overload; forward;
    
  /// Adds an item to the list where the items shows a cell of a
  /// bitmap and some text.
  ///
  /// @lib ListAddItemWithCellAndText
  /// @sn list:%s addBitmap:%s cell:%s andText:%s
  ///
  /// @class GUIList
  /// @overload AddItem AddItemWithCellAndText
  /// @csn addBitmap:%s cell:%s andText:%s
  procedure ListAddItem(lst: GUIList; img: Bitmap; cell: Longint; const text: String); overload; forward;
    
  /// Returns the index of the item with the bitmap, img
  ///
  /// @lib 
  /// @sn list:%s indexOfBitmap:%s
  ///
  /// @class GUIList
  /// @method BitmapItemIndex
  function ListBitmapIndex(lst: GUIList; img: Bitmap): Longint; overload; forward;

  /// Returns the index of the item with the bitmap and cell.
  ///
  /// @lib ListBitmapCellIndex
  /// @sn list:%s bitmapIndex:%s withCell:%s
  ///
  /// @class GUIList
  /// @method CellItemIndex
  function ListBitmapIndex(lst: GUIList; img: Bitmap; cell: Longint): Longint; overload; forward;

  /// returns the id of a value in the list.
  ///
  /// @lib
  /// @sn list:%s indexOfText:%s
  ///
  /// @class GUIList
  /// @method IndexOfTextItem
  function ListTextIndex(lst: GUIList; const value: String): Longint; forward;

  /// Returns the starting point for the list
  ///
  /// @lib
  ///
  /// @class GUIList
  /// @getter StartAt
  function ListStartAt(lst: GUIList): Longint; forward;

  /// Sets the starting point for the list
  ///
  /// @lib ListSetStartingAt
  /// @sn list:%s setStartAt:%s
  ///
  /// @class GUIList
  /// @setter StartAt
  procedure ListSetStartAt(lst: GUIList; idx: Longint); forward;

  /// Returns the largest index that startingAt should be set to.
  ///
  /// @lib ListLargestStartIndex
  ///
  /// @class GUIList
  /// @getter LargestStartIndex
  function ListLargestStartIndex(lst: GUIList): Longint; forward;

  /// Returns the largest index that startingAt should be set to.
  ///
  /// @lib
  ///
  /// @class GUIList
  /// @getter ScrollIncrement
  function ListScrollIncrement(lst: GUIList): Longint; forward;

  /// Get text From Label
  ///
  /// @lib
  /// @class Label
  /// @getter Text
  function  LabelText(lb: GUILabel): string; overload; forward;

  /// Set text for Label
  ///
  /// @lib 
  /// @sn label:%s setText:%s
  ///
  /// @class Label
  /// @setter Text
  procedure LabelSetText(lb: GUILabel; const newString: String); overload; forward;

  /// returns a GUILabel From given region
  ///
  /// @lib
  function  LabelFromRegion(r: RegionPtr): GUILabel; forward;



//
// Event management code
//

// This procedure calls the callbacks for the event
// This is private to the unit
procedure SendEvent(r: Region; kind: EventKind);
var
  i: Longint;
  pnl: PanelPtr;
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);

  if not assigned(rp) then exit;
  pnl := rp^.parent;
  if not assigned(pnl) then exit;
  
  for i := 0 to High(rp^.callbacks) do
  begin
    try
      rp^.callbacks[i](r, kind);
    except
      WriteLn(stderr, 'Error with event callback!');
    end;
  end;
end;







procedure HandlePanelInput(pnl: PanelPtr); forward;

function GUITextEntryComplete(): Boolean;
begin
  result := GUIC.doneReading;
end;

function GUITextBoxOfTextEntered(): GUITextbox;
begin
  result := nil;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := @GUIC.lastActiveTextBox^.parent^.textBoxes[GUIC.lastActiveTextBox^.elementIndex];
end;

function RegionOfLastUpdatedTextBox(): Region;
begin
  result := nil;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := GUIC.lastActiveTextBox;
end;

function IndexOfLastUpdatedTextBox(): Longint;
begin
  result := -1;
  if not assigned(GUIC.lastActiveTextBox) then exit;
  result := ToRegionPtr(RegionOfLastUpdatedTextBox())^.elementIndex;
end;

function ActiveTextIndex(): Longint;
begin
  result := -1;
  if not assigned(GUIC.activeTextBox) then exit;
  result := GUIC.activeTextBox^.elementIndex;
end;

procedure DeactivateTextBox();
begin 
  GUIC.activeTextBox := nil;
end;

function ActiveTextBoxParent() : Panel;
begin
  if not Assigned(GUIC.activeTextBox) then result := nil
  else result := GUIC.activeTextBox^.parent;
end;

function RegionRectangleOnscreen(r: Region): Rectangle;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  if not assigned(rp) then begin result := RectangleFrom(0,0,0,0); exit; end;
  
  result := RectangleOffset(rp^.area, RectangleTopLeft(rp^.parent^.area));
end;

function GUIClicked(): Boolean;
begin
  result := GUIC.panelClicked <> nil;
end;

function ModalBefore(pnl: PanelPtr): Boolean;
var
  i: Longint;
begin
  result := false;
  
  for i := High(GUIC.visiblePanels) downto 0 do
  begin
    if GUIC.visiblePanels[i] = pnl then exit
    else if GUIC.visiblePanels[i]^.modal then
    begin
      result := true;
      exit;
    end;
  end;
end;

//---------------------------------------------------------------------------------------
// Drawing Loops
//---------------------------------------------------------------------------------------  

function BitmapToDraw(r: RegionPtr): Bitmap;
begin
  result := nil;
  if not(assigned(r)) then exit;
  
  // Check if the panel or the region is inactive
  if (not r^.parent^.active) or (not r^.active) then 
  begin
    result := r^.parent^.panelBitmapInactive; 
    exit; 
  end;
  
  // Check based upon the kind of the region
  case r^.kind of
    gkButton:
    begin
      if MouseDown(LeftButton) AND PointInRect(MousePosition(), RegionRectangleOnScreen(r)) then
        result := r^.parent^.panelBitmapActive
      else
        result := nil; // dont redraw the r^.parent^.panelBitmap;
    end;
    gkCheckBox:
    begin
      if CheckboxState(r) then 
       result := r^.parent^.panelBitmapActive
     else
       result := nil; // dont redraw the 
    end;
    gkRadioGroup: 
    begin
      if r = ActiveRadioButton(RadioGroupFromRegion(r)) then
        result := r^.parent^.panelBitmapActive
      else
        result := nil; // dont redraw the 
    end;
  end;
end;

//Helper functions used to determine the text rectangle for text boxes
function TextboxTextArea(const r: Rectangle): Rectangle; overload;
begin
  result := InsetRectangle(r, 1);
end;

function TextboxTextArea(r: Region): Rectangle; overload;
begin
  result := TextboxTextArea(RegionRectangleOnScreen(r));
end;

function VectorForecolorToDraw(r: RegionPtr): Color;
begin
  result := GUIC.foregroundClr;
  if not(assigned(r)) then exit;
  
  // Check if the panel or the region is inactive
  if (not r^.parent^.active) or (not r^.active) then 
  begin
    result := GUIC.foregroundClrInactive; 
    exit; 
  end;
  
  // Change the button on mouse down
  case r^.kind of
    gkButton:
    begin
      if MouseDown(LeftButton) AND PointInRect(MousePosition(), RegionRectangleOnScreen(r)) then
        result := guic.backgroundClr
    end;
  end;
end;

function VectorBackcolorToDraw(r: RegionPtr): Color;
begin
  result := GUIC.backgroundClr;
  if not(assigned(r)) then exit;
  
  // Check if the panel or the region is inactive
  if (not r^.parent^.active) or (not r^.active) then 
  begin
    result := GUIC.backgroundClrInactive; 
    exit; 
  end;
  
  // Change the button on mouse down
  case r^.kind of
    gkButton:
    begin
      if MouseDown(LeftButton) AND PointInRect(MousePosition(), RegionRectangleOnScreen(r)) then
        result := guic.foregroundClr
    end;
  end;
end;

procedure DrawVectorCheckbox(forRegion: RegionPtr; const area: Rectangle);
begin
  DrawRectangle(VectorForecolorToDraw(forRegion), area, OptionToScreen());
  
  if CheckboxState(forRegion) then 
  begin
    FillRectangle(VectorForecolorToDraw(forRegion), InsetRectangle(area, 2), OptionToScreen());
  end;
end;

procedure DrawVectorRadioButton(forRegion: RegionPtr; const area: Rectangle);
begin
  DrawEllipse(VectorForecolorToDraw(forRegion), area, OptionToScreen());
  
  if forRegion = ActiveRadioButton(RadioGroupFromRegion(forRegion)) then
  begin
    FillEllipse(VectorForecolorToDraw(forRegion), InsetRectangle(area, 2), OptionToScreen());
  end;
end;

procedure DrawTextbox(forRegion: RegionPtr; const area: Rectangle);
begin
  if not Assigned(forRegion) then exit;

  if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then DrawRectangle(VectorForecolorToDraw(forRegion), area, OptionToScreen());
  
  if GUIC.activeTextBox <> forRegion then
    DrawText(TextboxText(forRegion), 
            VectorForecolorToDraw(forRegion),
            VectorBackcolorToDraw(forRegion), 
            forRegion^.font, 
            forRegion^.alignment,
            TextboxTextArea(area),
            OptionToScreen());
end;
  
procedure DrawList(forRegion: RegionPtr; const area: Rectangle);
var
  tempList:               GUIList;
  i, itemIdx:             Longint;
  areaPt, imagePt:        Point2D;
  itemArea, scrollArea:   Rectangle;
  itemTextArea:           Rectangle;
  placeHolderScreenRect:  Rectangle;

  procedure _DrawUpDownArrow(const rect: Rectangle; up: Boolean);
  var
    tri: Triangle;
    innerRect, arrowArea: Rectangle;
  begin
    arrowArea := RectangleOffset(rect, areaPt);
    FillRectangle(VectorForecolorToDraw(forRegion), arrowArea, OptionToScreen());
    innerRect := InsetRectangle(arrowArea, 2);
    
    if up then
    begin
      tri.points[0] := RectangleBottomLeft(innerRect);
      tri.points[1] := RectangleBottomRight(innerRect);
      tri.points[2] := RectangleCenterTop(innerRect);
    end
    else
    begin
      tri.points[0] := RectangleTopLeft(innerRect);
      tri.points[1] := RectangleTopRight(innerRect);
      tri.points[2] := RectangleCenterBottom(innerRect);
    end;
    
    FillTriangle(VectorBackcolorToDraw(forRegion), tri, OptionToScreen());
  end;
  procedure _DrawLeftRightArrow(const rect: Rectangle; left: Boolean);
  var
    tri: Triangle;
    innerRect, arrowArea: Rectangle;
  begin
    arrowArea := RectangleOffset(rect, areaPt);
    FillRectangle(VectorForecolorToDraw(forRegion), arrowArea, OptionToScreen());
    innerRect := InsetRectangle(arrowArea, 2);
    
    if left then
    begin
      tri.points[0] := RectangleCenterLeft(innerRect);
      tri.points[1] := RectangleBottomRight(innerRect);
      tri.points[2] := RectangleTopRight(innerRect);
    end
    else
    begin
      tri.points[0] := RectangleCenterRight(innerRect);
      tri.points[1] := RectangleTopLeft(innerRect);
      tri.points[2] := RectangleBottomLeft(innerRect);
    end;
    
    FillTriangle(VectorBackcolorToDraw(forRegion), tri, OptionToScreen());
  end;
  
  procedure _ResizeItemArea(var area: Rectangle; var imgPt: Point2D; aligned: FontAlignment; bmp: Bitmap);
  begin
    
    case aligned of
      AlignCenter:
      begin
        imgPt.x := imgPt.x + (area.Width - BitmapCellWidth(bmp)) / 2.0;
      end;
      AlignLeft:
      begin
        area.Width := area.Width - BitmapCellWidth(bmp) - 1; // 1 pixel boundry for bitmap
        area.x     := area.x + BitmapCellWidth(bmp);
        imgPt.x    := imgPt.x + 1;
      end;
      AlignRight:
      begin
        area.Width := area.Width - BitmapCellWidth(bmp) - 1;
        imgPt.x    := imgPt.x + area.width + 1;
      end;
    end;
  end;
  
  procedure _DrawScrollPosition();
  var
    pct:              Single;
    largestStartIdx:  Longint;
  begin
    largestStartIdx := ListLargestStartIndex(tempList);
    
    // if the number of items is >= the number shown then pct := 0
    if largestStartIdx <= 0 then 
      pct := 0
    else 
    begin
      pct := Single(tempList^.startingAt / largestStartIdx);
    end;
    
    if tempList^.verticalScroll then
    begin
      if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
        FillRectangle(VectorForecolorToDraw(forRegion), 
                              RoundInt(scrollArea.x),
                              RoundInt(scrollArea.y + pct * (scrollArea.Height - tempList^.scrollSize)),
                              tempList^.scrollSize,
                              tempList^.scrollSize,
                              OptionToScreen()
                              )
      else
        DrawBitmap(tempList^.ScrollButton,
                  RoundInt(scrollArea.x),
                  RoundInt(scrollArea.y + pct * (scrollArea.Height - tempList^.scrollSize)),
                  OptionToScreen());
    end
    else
    begin
      if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
        FillRectangle(VectorForecolorToDraw(forRegion), 
                      RoundInt(scrollArea.x + pct * (scrollArea.Width - tempList^.scrollSize)),
                      RoundInt(scrollArea.y),
                      tempList^.scrollSize,
                      tempList^.scrollSize,
                      OptionToScreen())
      else
        DrawBitmap(tempList^.ScrollButton,
                            RoundInt(scrollArea.x + pct * (scrollArea.Width - tempList^.scrollSize)),
                            RoundInt(scrollArea.y),
                            OptionToScreen());
    end;
  end;
begin
  tempList := ListFromRegion(forRegion);
  if not assigned(tempList) then exit;
  
  if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then DrawRectangle(VectorForecolorToDraw(forRegion), area, OptionToScreen());
  
  PushClip(area);
  areaPt := RectangleTopLeft(area);
  
  // Draw the up and down buttons
  if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
  begin
    if tempList^.verticalScroll then
    begin
      _DrawUpDownArrow(tempList^.scrollUp, true);
      _DrawUpDownArrow(tempList^.scrollDown, false);
    end
    else
    begin
      _DrawLeftRightArrow(tempList^.scrollUp, true);
      _DrawLeftRightArrow(tempList^.scrollDown, false);    
    end;
  end;
  
  // Draw the scroll area
  scrollArea := RectangleOffset(tempList^.scrollArea, areaPt);
  
  if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
  begin
    DrawRectangle(VectorBackcolorToDraw(forRegion), scrollArea, OptionToScreen());
    DrawRectangle(VectorForecolorToDraw(forRegion), scrollArea, OptionToScreen());
  end;
  
  // Draw the scroll position indicator
  PushClip(scrollArea);
  _DrawScrollPosition();
  PopClip(); // pop the scroll area
  
  //Draw all of the placeholders
  for i := Low(tempList^.placeHolder) to High(tempList^.placeHolder) do
  begin
    itemTextArea          := RectangleOffset(tempList^.placeHolder[i], areaPt);
    itemArea              := RectangleOffset(tempList^.placeHolder[i], areaPt);
    placeHolderScreenRect := RectangleOffset(tempList^.placeHolder[i], RectangleTopLeft(forRegion^.area));
    
    // Outline the item's area
    if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
      DrawRectangle(VectorForecolorToDraw(forRegion), itemArea, OptionToScreen());
    
    // Find the index of the first item to be shown in the list
    // NOTE: place holders in col then row order 0, 1 -> 2, 3 -> 4, 5 (for 2 cols x 3 rows)
    
    if tempList^.verticalScroll then
      itemIdx := i + tempList^.startingAt
    else
      // 0, 1 => 0, 3
      // 2, 3 => 1, 4
      // 4, 5 => 2, 5
      itemIdx := tempList^.startingAt + ((i mod tempList^.columns) * tempList^.rows) + (i div tempList^.columns);
      
    // Dont draw item details if out of range, but continue to draw outlines
    if (itemIdx < 0) OR (itemIdx > High(tempList^.items)) then continue;
    
    PushClip(itemArea);
    
    //Draw the placeholder background
    
    
    //Draw the text (adjusting position for width of list item bitmap)
    if assigned(tempList^.items[itemIdx].image) then
    begin
      // Determine the location of the list item's bitmap
      imagePt   := RectangleTopLeft(itemArea);    
      imagePt.y := imagePt.y + (itemArea.height - BitmapCellHeight(tempList^.items[itemidx].image)) / 2.0;
      
      _ResizeItemArea(itemTextArea, imagePt, forRegion^.alignment, tempList^.items[itemIdx].image);
    end;
    
    // if selected draw the alternate background
    if (itemIdx = tempList^.activeItem) then
    begin
      if forRegion^.parent^.DrawAsVectors or GUIC.VectorDrawing then
      begin
        // Fill and draw text in alternate color if vector based
        FillRectangle(VectorForecolorToDraw(forRegion), itemArea, OptionToScreen());
      end
      else
      begin
        // Draw bitmap and text if bitmap based
        DrawBitmap(forRegion^.parent^.panelBitmapActive,
                       RoundInt(RectangleTopLeft(itemArea).x),
                       RoundInt(RectangleTopLeft(itemArea).y),
                       OptionToScreen(OptionPartBmp(placeHolderScreenRect.x, placeHolderScreenRect.y, placeHolderScreenRect.width, placeHolderScreenRect.height)));
      end;
    end;
    
    // Draw the item's bitmap
    if  assigned(tempList^.items[itemIdx].image) then
    begin
      DrawCell(tempList^.items[itemIdx].image, tempList^.items[itemIdx].cell, imagePt.x, imagePt.y, OptionToScreen());
    end;
    
    // Draw the text on top
    if (itemIdx <> tempList^.activeItem) then
    begin
      // Draw the text onto the screen as normal...
      DrawText(ListItemText(tempList, itemIdx), 
               VectorForecolorToDraw(forRegion), 
               ColorTransparent, 
               forRegion^.font, 
               forRegion^.alignment, 
               itemTextArea,
               OptionToScreen());
    end
    else // the item is the selected item...
    begin
      DrawText(ListItemText(tempList, itemIdx), 
               VectorBackcolorToDraw(forRegion), 
               ColorTransparent,
               forRegion^.font, 
               forRegion^.alignment,
               itemTextArea,
               OptionToScreen());
    end;
    
    PopClip(); // item area
  end;
  
  PopClip(); // region
end;

procedure DrawLabelText(forRegion: RegionPtr; const area: Rectangle);
begin
  DrawText(LabelText(forRegion), 
           VectorForecolorToDraw(forRegion),
           ColorTransparent, //VectorBackcolorToDraw(forRegion), 
           forRegion^.font,
           forRegion^.alignment, 
           TextboxTextArea(area),
           OptionToScreen());
end;

procedure DrawAsVectors(p: PanelPtr);
var
  j: LongInt;
  current: PanelPtr;
  currentReg: RegionPtr;
begin
  current := p;
  if not assigned(current) then exit;

  if current^.active then
  begin
    FillRectangle(GUIC.backgroundClr, current^.area, OptionToScreen());
    DrawRectangle(GUIC.foregroundClr, current^.area, OptionToScreen());
  end
  else
  begin
    FillRectangle(GUIC.backgroundClrInactive, current^.area, OptionToScreen());
    DrawRectangle(GUIC.foregroundClrInactive, current^.area, OptionToScreen());
  end;
  
  PushClip(current^.area);
  
  for j := High(current^.Regions) downto Low(current^.Regions) do
  begin
    currentReg := @p^.Regions[j];
    case currentReg^.kind of
      gkButton:     DrawRectangle(VectorForecolorToDraw(currentReg), RegionRectangleOnscreen(currentReg), OptionToScreen());
      gkLabel:      DrawLabelText(currentReg, RegionRectangleOnscreen(currentReg));
      gkCheckbox:   DrawVectorCheckbox(currentReg, RegionRectangleOnScreen(currentReg));
      gkRadioGroup: DrawVectorRadioButton(currentReg, RegionRectangleOnScreen(currentReg));
      gkTextbox:    DrawTextbox(currentReg, RegionRectangleOnScreen(currentReg));
      gkList:       DrawList(currentReg, RegionRectangleOnScreen(currentReg));
    end;
  end;
  PopClip();
end;

function PanelBitmapToDraw(p: PanelPtr): Bitmap;
begin
  if not PanelActive(p) then
  begin
    result := p^.PanelBitmapInactive;
    exit;
  end;
  
  result := p^.PanelBitmap;
end;

procedure DrawAsBitmaps(p: PanelPtr);
var
  j: LongInt;
  currentReg: RegionPtr;
begin
  DrawBitmap(PanelBitmapToDraw(p), RoundInt(RectangleTopLeft(p^.area).x), RoundInt(RectangleTopLeft(p^.area).y), OptionToScreen());
  
  for j := Low(p^.Regions) to High(p^.Regions) do
  begin
    currentReg := @p^.Regions[j];
    case p^.Regions[j].kind of
      gkLabel:      DrawLabelText(currentReg, RegionRectangleOnscreen(currentReg));
      gkTextbox:    DrawTextbox(currentReg, RegionRectangleOnScreen(currentReg));
      gkList:       DrawList(currentReg, RegionRectangleOnScreen(currentReg));
      else          DrawBitmap(BitmapToDraw(currentReg), RoundInt(RectangleTopLeft(RegionRectangleOnScreen(currentReg)).x), RoundInt(RectangleTopLeft(RegionRectangleOnScreen(currentReg)).y), OptionToScreen(OptionPartBmp(currentReg^.area.x, currentReg^.area.y, currentReg^.area.width, currentReg^.area.height)));
    end;
  end;
end;
  
procedure DrawInterface();
var
  i: longint;
begin
  for i := Low(GUIC.visiblePanels) to High(GUIC.visiblePanels) do
  begin  
    
    if GUIC.visiblePanels[i]^.DrawAsVectors or GUIC.VectorDrawing then
    begin
      DrawAsVectors(GUIC.visiblePanels[i])
    end
    else
    begin
      DrawAsBitmaps(GUIC.visiblePanels[i]);
    end;
  end;
end;

//---------------------------------------------------------------------------------------
// Region Code
//---------------------------------------------------------------------------------------

function RegionWidth(r: Region): Longint;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  result := -1;
  if not(Assigned(rp)) then exit;
  
  result := Round(rp^.area.width);
end;

function RegionHeight(r: Region): Longint;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  result := -1;
  if not(assigned(rp)) then exit;
  
  result := Round(rp^.area.height);
end;

function RegionX(r: Region): Single;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  result := -1;
  
  if not(assigned(rp)) then exit;
  
  result := rp^.area.x;
end;

function RegionY(r: Region): Single;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  result := -1;
  if not(assigned(rp)) then exit;
  
  result := rp^.area.y;
end;

procedure SetRegionActive(forRegion: Region; b: Boolean);
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(forRegion);

  if not assigned(rp) then exit;
  rp^.active := b;
end;

function RegionActive(forRegion: Region): Boolean;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(forRegion);

  if not assigned(rp) then result := false
  else result := rp^.active;
end;

procedure ToggleRegionActive(forRegion: Region);
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(forRegion);

  if not assigned(rp) then exit;
  rp^.active := not rp^.active;
end;

function RegionWithID(pnl: Panel; const ID: String): Region; overload;
var
  idx: Longint;
  pp: PanelPtr;
begin
  pp := ToPanelPtr(pnl);

  result := nil;
  if not assigned(pp) then exit;
  
  idx := IndexOf(pp^.regionIds, ID);
  if idx >= 0 then
  begin
    result := @pp^.regions[idx];
  end;  
end;

function RegionWithID(const ID: String): Region; overload;
var
  i: LongInt;
begin
  result := nil;
  
  for i := Low(GUIC.panels) to High(GUIC.panels) do
  begin
    result := RegionWithID(GUIC.panels[i], ID);
    if assigned(result) then exit;
  end;
end;

function RegionID(r: Region): string;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);

  if assigned(rp) then
    result := rp^.stringID
  else
    result := '';
end;

// Check which of the regions was clicked in this panel
procedure HandlePanelInput(pnl: PanelPtr);
  procedure _ScrollWithScrollArea(lst: GUIList; const mouse: Point2D);
  var
    largestStartIdx: Longint;
    pct: Single;
  begin
    GUIC.lastClicked := nil;
    largestStartIdx := ListLargestStartIndex(lst);
    
    if lst^.verticalScroll then
      pct := (mouse.y - lst^.scrollArea.y) / lst^.scrollArea.height
    else
      pct := (mouse.x - lst^.scrollArea.x) / lst^.scrollArea.width;
    
    lst^.startingAt := RoundInt(pct * largestStartIdx / ListScrollIncrement(lst)) * ListScrollIncrement(lst);
    exit;
  end;
  
  procedure _ScrollUp(lst: GUIList);
  var
    inc: Longint;
  begin
    inc := ListScrollIncrement(lst);
    
    if lst^.startingAt >= inc then 
      lst^.startingAt := lst^.startingAt - inc;
  end;
  
  procedure _ScrollDown(lst: GUIList);
  var
    inc, largestStartIdx: Longint;
  begin
    inc := ListScrollIncrement(lst);
    largestStartIdx := ListLargestStartIndex(lst);
    
    if lst^.startingAt + inc <= largestStartIdx then
      lst^.startingAt := lst^.startingAt + inc;
  end;
  
  procedure _PerformListClicked(lstRegion: Region; lst: GUIList; const pointClicked: Point2D);
  var
    i: Longint;
  begin
    if not assigned(lst) then exit;
    
    // WriteLn(PointToString(pointClicked));
    // Write(lst^.startingAt);
    
    // itemCount  := Length(lst^.items);
    // placeCount := lst^.rows * lst^.columns;
    
    // Check up and down scrolling...
    if PointInRect(pointClicked, lst^.scrollUp) then
    begin
      _ScrollUp(lst);
      // WriteLn(' -> ', lst^.startingAt);
      GUIC.lastClicked := nil; // click in scroller...
      exit;
    end
    else if PointInRect(pointClicked, lst^.scrollDown) then
    begin
      _ScrollDown(lst);
      // WriteLn(' -> ', lst^.startingAt);
      GUIC.lastClicked := nil; // click in scroller...
      exit;
    end
    else if PointInRect(pointClicked, lst^.scrollArea) then
    begin
      _ScrollWithScrollArea(lst, pointClicked);
      exit;
    end;
    
    // WriteLn(' -> ', lst^.startingAt);
    
    for i := Low(lst^.placeHolder) to High(lst^.placeHolder) do
    begin
      if PointInRect(pointClicked, lst^.placeHolder[i]) AND (lst^.startingAt + i < Length(lst^.items)) then
      begin
        lst^.activeItem := lst^.startingAt + i;
        SendEvent(lstRegion, ekSelectionMade);
        exit;
      end;
    end;
  end;
  
  procedure _PerformMouseDownOnList(lst: GUIList; const mouse: Point2D);
  begin
    if PointInRect(mouse, lst^.scrollArea) then
    begin
      _ScrollWithScrollArea(lst, mouse);
    end;
  end;
  
  procedure _UpdateMouseDown(const mouse: Point2D);
  var
    pointDownInRegion: Point2D;
    r: RegionPtr;
  begin
    if ReadingText() then
      FinishReadingText();
    
    r := RegionPtrAtPoint(pnl, mouse);
    if not assigned(r) then exit;
    
    // Adjust the mouse point into the region's coordinates (offset from top left of region)
    pointDownInRegion := PointAdd(mouse, InvertVector(RectangleTopLeft(r^.area)));
    
    // Perform kind based updating
    case r^.kind of
      gkList: _PerformMouseDownOnList(ListFromRegion(r), pointDownInRegion);
    end;
  end;
  
  procedure _UpdateScrollUp(const mouse: Point2D);
  var
    r: RegionPtr;
  begin
    r := RegionPtrAtPoint(pnl, mouse);
    if not assigned(r) then exit;
    
    // Perform kind based updating
    case r^.kind of
      gkList: _ScrollUp(ListFromRegion(r));
    end;
  end;
  
  procedure _UpdateScrollDown(const mouse: Point2D);
  var
    r: RegionPtr;
  begin
    r := RegionPtrAtPoint(pnl, mouse);
    if not assigned(r) then exit;
    
    // Perform kind based updating
    case r^.kind of
      gkList: _ScrollDown(ListFromRegion(r));
    end;
  end;
  
  procedure _UpdateMouseClicked(const pointClicked: Point2D);
  var
    pointClickedInRegion: Point2D;
    r: RegionPtr;
  begin
    if ReadingText() then
      FinishReadingText();
    
    r := RegionPtrAtPoint(pnl, pointClicked);
    if not assigned(r) then exit;
    if not r^.active then exit;
    
    GUIC.lastClicked := r;
    
    // Adjust the mouse point into the region's coordinates (offset from top left of region)
    pointClickedInRegion := PointAdd(pointClicked, InvertVector(RectangleTopLeft(r^.area)));
    
    // Perform kind based updating
    case r^.kind of
      gkCheckBox:     ToggleCheckboxState (CheckboxFromRegion(r) );
      gkRadioGroup:   SelectRadioButton   (RadioGroupFromRegion(r), r );
      gkTextBox:      GUISetActiveTextbox (TextBoxFromRegion(r) );
      gkList:         _PerformListClicked (r, ListFromRegion(r), pointClickedInRegion);
    end;
    
    if assigned(GUIC.lastClicked) then SendEvent(GUIC.lastClicked, ekClicked);
  end;

var
  pointClickedInPnl: Point2D;
begin
  if pnl = nil then exit;
  if not PanelActive(pnl) then exit;
  
  // Adjust the mouse point into this panels area (offset from top left of pnl)
  pointClickedInPnl := PointAdd(MousePosition(), InvertVector(RectangleTopLeft(pnl^.area)));
  
  // Handle mouse interaction
  if MouseClicked(LeftButton)   then _UpdateMouseClicked(pointClickedInPnl)
  else if MouseDown(LeftButton) then _UpdateMouseDown(pointClickedInPnl)
  else if MouseClicked(WheelUpButton)   then _UpdateScrollUp(pointClickedInPnl)
  else if MouseClicked(WheelDownButton) then _UpdateScrollDown(pointClickedInPnl);
end;

function RegionClicked(): Region;
begin   
  result := Region(GUIC.lastClicked);
end;

function RegionClickedID(): String;
begin
  result := RegionID(GUIC.lastClicked);
end;

function RegionPanel(r: Region): Panel;
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  result := nil;
  if not assigned(rp) then exit;
  
  result := rp^.parent;
end;

function RegionPtrAtPoint(p: PanelPtr; const pt: Point2D): RegionPtr;
var
  i: Longint;
  current: RegionPtr;
begin
  result := nil;
  if not assigned(p) then exit;
  
  for i := Low(p^.Regions) to High(p^.Regions) do
  begin
    // Get the region at the i index
    current := @p^.Regions[i];
    
    //Check if it has been clicked
    if PointInRect(pt, current^.area) then
    begin
      result := current;
      exit;
    end;
  end;
end;

function RegionAtPoint(p: Panel; const pt: Point2D): Region;
begin
  result := Region(RegionPtrAtPoint(ToPanelPtr(p), pt));
end;

//---------------------------------------------------------------------------------------
// Get element from region
//---------------------------------------------------------------------------------------

function RadioGroupFromRegion(r: RegionPtr): GUIRadioGroup;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkRadioGroup) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.radioGroups)) then exit;

  result := @r^.parent^.radioGroups[r^.elementIndex];
end;

function RadioGroupFromId(pnl: PanelPtr; const id: String): GUIRadioGroup;
var
  i: Longint;
  lid: String;
begin
  result := nil;
  if not assigned(pnl) then exit;
  
  lid := LowerCase(id);
  
  for i := 0 to High(pnl^.radioGroups) do
  begin
    if LowerCase(pnl^.radioGroups[i].groupID) = lid then
    begin
      result := @pnl^.radioGroups[i];
      exit;
    end;
  end;
end;

function RadioGroupFromId(const id : String): GUIRadioGroup;
begin
  result := RadioGroupFromRegion(RegionWithID(id));
end;

function LabelFromRegion(r: RegionPtr): GUILabel;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkLabel) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.labels)) then exit;
  
  //Return a pointer to the label in the panel
  result := @r^.parent^.labels[r^.elementIndex]
end;

function TextBoxFromRegion(r: RegionPtr): GUITextBox;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkTextbox) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.textBoxes)) then exit;
  
  result := @r^.parent^.textBoxes[r^.elementIndex]
end;

function ListFromRegion(r: RegionPtr): GUIList; overload;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkList) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.lists)) then exit;
  
  result := @r^.parent^.lists[r^.elementIndex]
end;

function CheckboxFromRegion(r: RegionPtr): GUICheckbox;
begin
  result := nil;
  
  if not assigned(r) then exit;
  if not assigned(r^.parent) then exit;
  if not (r^.kind = gkCheckBox) then exit;
  if (r^.elementIndex < 0) or (r^.elementIndex > High(r^.parent^.checkboxes)) then exit;
  
  result := @r^.parent^.checkboxes[r^.elementIndex];
end;


//---------------------------------------------------------------------------------------
// RadioGroup Code
//---------------------------------------------------------------------------------------

function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): LongInt;
begin
  result := -1;
  if not(assigned(RadioGroup)) then exit;
  result := RadioGroup^.activeButton;
end;

{function ActiveRadioButton(r: Region): Region;
begin
  result := ActiveRadioButton(RadioGroupFromRegion(r));
end;
}

function ActiveRadioButtonIndex(pnl : Panel; const id : String): LongInt;
begin
  result := ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID(pnl,id)));
end;

function ActiveRadioButtonIndex(const id : String): LongInt;
begin
  result := ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID(id)));
end;

function ActiveRadioButton(grp: GUIRadioGroup): Region;
begin
  result := nil;
  if not assigned(grp) then exit;
  if (grp^.activeButton < 0) or (grp^.activeButton > High(grp^.buttons)) then exit;
  
  result := grp^.buttons[grp^.activeButton];
end;

function ActiveRadioButton(pnl : Panel; const id : String): Region;
begin
  result := ActiveRadioButton(RadioGroupFromRegion(RegionWithID(pnl,id)));
end;

function ActiveRadioButton(const id : String): Region;
begin
  result := ActiveRadioButton(RadioGroupFromRegion(RegionWithID(id)));
end;


procedure SelectRadioButton(r: Region); overload;
begin
  SelectRadioButton(RadioGroupFromRegion(r), r);
end;

procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;
var
  i: LongInt;
begin
  if not assigned(rGroup) then exit;
  if RadioGroupFromRegion(r) <> rGroup then exit;
    
  // Remove the selection ...
  rGroup^.activeButton := -1;
  
  // Find this button in the group and select it
  for i := Low(rGroup^.buttons) to High(rGroup^.buttons) do
  begin
    if rGroup^.buttons[i] = r then
    begin
      rGroup^.activeButton := i;
      exit;
    end;
  end;
end;

procedure SelectRadioButton(const id : String);
begin
  SelectRadioButton(RegionWithID(id));
end;

procedure SelectRadioButton(pnl : Panel; const id : String);
begin
  SelectRadioButton(RegionWithID(pnl, id));
end;

procedure SelectRadioButton(rGroup: GUIRadioGroup; idx: LongInt);
begin
    if not assigned(rGroup) then exit;
    if ((idx < 0) or (idx > High(rGroup^.buttons))) then exit;
    rGroup^.activeButton := idx;
end;

//---------------------------------------------------------------------------------------
// Label Code
//---------------------------------------------------------------------------------------

procedure LabelSetText(const id, newString: String); overload;
begin
  LabelSetText(LabelFromRegion(RegionWithID(id)), newString);
end;

procedure LabelSetText(pnl: Panel; const id, newString: String); overload;
begin
  LabelSetText(LabelFromRegion(RegionWithID(pnl, id)), newString);
end;

procedure LabelSetText(r: Region; const newString: String); overload;
begin
  LabelSetText(LabelFromRegion(r), newString);
end;

procedure LabelSetText(lb: GUILabel; const newString: String);
begin
  if not assigned(lb) then exit;  
  
  lb^.contentString := newString;
end;

procedure RegionSetFont(r: Region; f: Font);
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  if not assigned(rp) then exit;
  rp^.font := f;
end;

function RegionFont(r: Region): Font;
var
  rp: RegionPtr;
begin
  result := nil;
  rp := ToRegionPtr(r);
  if not assigned(rp) then exit;
  
  result := rp^.font;
end;

function RegionFontAlignment(r: Region): FontAlignment;
var
  rp: RegionPtr;
begin
  result := AlignLeft;
  rp := ToRegionPtr(r);
  if not assigned(rp) then exit;
  
  result := rp^.alignment;
end;

procedure RegionSetFontAlignment(r: Region; align: FontAlignment);
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  if not assigned(rp) then exit;
  
  rp^.alignment := align;
end;

function LabelText(lb: GUILabel): String; overload;
begin
  if assigned(lb) then
    result := lb^.contentString
  else
    result := '';
end;

function LabelText(r: Region): String; overload;
begin
  result := LabelText(LabelFromRegion(r));
end;

function LabelText(const id : String): String; overload;
begin
  result := LabelText(LabelFromRegion(RegionWithID(id)));
end;

function LabelText(pnl : Panel; const id : String): String; overload;
begin
  result := LabelText(LabelFromRegion(RegionWithID(pnl,id)));
end;

//---------------------------------------------------------------------------------------
// Textbox Code
//---------------------------------------------------------------------------------------

procedure TextboxSetText(const id, s: string); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(id)), s);
end;

procedure TextboxSetText(const id : String; i: Longint); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(id)), IntToStr(i));
end;

procedure TextboxSetText(const id : String; single: Single); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(id)), FloatToStr(single));
end;

procedure TextboxSetText(pnl : Panel; const id, s: string); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(pnl,id)), s);
end;

procedure TextboxSetText(pnl : Panel; const id : String; i: Longint); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(pnl,id)), IntToStr(i));
end;

procedure TextboxSetText(pnl : Panel; const id : String; single: Single); overload;
begin
  TextboxSetText(TextBoxFromRegion(RegionWithId(pnl,id)), FloatToStr(single));
end;


procedure TextboxSetText(r: Region; const s: string); overload;
begin
  TextboxSetText(TextBoxFromRegion(r), s);
end;

procedure TextboxSetText(r: Region; i: Longint); overload;
begin
  TextboxSetText(TextBoxFromRegion(r), IntToStr(i));
end;

procedure TextboxSetText(r: Region; single: Single); overload;
begin
  TextboxSetText(TextBoxFromRegion(r), FloatToStr(single));
end;

procedure TextboxSetText(tb: GUITextBox; const s: string); overload;
begin
  if assigned(tb) then
    tb^.contentString := s;
end;

procedure TextboxSetText(tb: GUITextBox; i: Longint); overload;
begin
  TextboxSetText(tb, IntToStr(i));
end;

procedure TextboxSetText(tb: GUITextBox; single: Single); overload;
begin
  TextboxSetText(tb, FloatToStr(single));
end;

function TextBoxText(const id: String): String; overload;
begin
  result := TextBoxText(RegionWithID(id));
end;

function TextBoxText(pnl : Panel; const id: String): String; overload;
begin
  result := TextBoxText(RegionWithID(pnl, id));
end;

function TextBoxText(r: Region): String; overload;
begin
  result := TextBoxText(TextBoxFromRegion(r));
end;

function TextBoxText(tb: GUITextBox): String; overload;
begin
  result := '';
  if not assigned(tb) then exit;
  
  result := tb^.contentString;
end;

/// The the TextBox from an ID
///
/// @lib TextBoxFromId
function TextBoxFromId(const id : String) : GUITextbox;
begin
  result := TextBoxFromRegion(RegionWithId(id));
end;

function LastTextRead(): string;
begin
  result := GUIC.lastTextRead;
end;

procedure FinishReadingText();
begin
  if not assigned(GUIC.activeTextBox) then exit;
  
  // Only perform an entry complete if this wasn't cancelled
  if not TextEntryCancelled() then
  begin
    // Read the old value of the text box
    GUIC.lastTextRead := TextBoxText(GUIC.activeTextBox);
    
    // assign the new value
    TextboxSetText(GUIC.activeTextBox, EndReadingText());
    
    // Which was the last active textbox - so we know where data was entered
    GUIC.lastActiveTextBox := GUIC.activeTextBox;
    
    GUIC.doneReading := true;
    
    SendEvent(GUIC.activeTextBox, ekTextEntryEnded);
  end;
  
  GUIC.activeTextBox := nil;
end;

procedure GUISetActiveTextbox(r: Region); overload;
begin
  GUISetActiveTextbox(TextBoxFromRegion(r));
end;

procedure GUISetActiveTextbox(const name: String); overload;
begin
  GUISetActiveTextbox(TextBoxFromRegion(RegionWithId(name)));
end;

procedure GUISetActiveTextbox(t: GUITextbox); overload;
begin
  if not assigned(t) then exit;
  
  GUIC.activeTextBox := t^.forRegion;
  
  if ReadingText() then FinishReadingText();
  StartReadingTextWithText( t^.contentString, 
                            GUIC.foregroundClr,
                            GUIC.backgroundClr, 
                            t^.lengthLimit, 
                            t^.forRegion^.Font, 
                            InsetRectangle(TextboxTextArea(t^.forRegion), 1)); // Need to inset 1 further to match printing text lines
end;

//---------------------------------------------------------------------------------------
// List Code
//---------------------------------------------------------------------------------------

function ListActiveItemText(list: GUIList): String; overload;
begin
  result := ListItemText(list, ListActiveItemIndex(list));
end;

function ListActiveItemText(const ID: String): String; overload;
begin
  result := ListActiveItemText(RegionWithID(id));
end;

function ListActiveItemText(pnl: Panel; const ID: String): String; overload;
var
  r: Region;
begin
  result := '';
  if not assigned(pnl) then exit;
  r := RegionWithID(pnl, ID);

  Result := ListItemText(r, ListActiveItemIndex(r));
end;

function ListActiveItemText(r: Region):String; Overload;
begin
  Result := ListItemText(r, ListActiveItemIndex(r));
end;


procedure ListSetActiveItemIndex(lst: GUIList; idx: Longint);
begin
  if not assigned(lst) then exit;
  lst^.activeItem := idx;
end;

procedure ListSetActiveItemIndex(pnl : Panel; const id : String; idx : LongInt);
begin
  ListSetActiveItemIndex(ListFromRegion(RegionWithID(pnl, id)),idx);
end;

procedure ListSetActiveItemIndex(const id : String; idx : LongInt);
begin
  ListSetActiveItemIndex(ListFromRegion(RegionWithID(id)),idx);
end;

function ListItemText(r: Region; idx: Longint): String; overload;
begin
  result := ListItemText(ListFromRegion(r), idx);
end;

function ListItemText(lst: GUIList; idx: Longint): String; overload;
begin
  result := '';
  if not assigned(lst) then exit;
  if (idx < 0) or (idx > High(lst^.items)) then exit;
  
  result := lst^.items[idx].text;
end;

function ListItemText(const id: String; idx: Longint): String; overload;
begin
  result := ListItemText(RegionWithID(id),idx);
end;

function ListItemText(pnl : Panel; const id: String; idx: Longint): String; overload;
begin
  result := ListItemText(RegionWithID(pnl,id),idx);
end;

function ListItemCount(r: Region): Longint; overload;
begin
  result := ListItemCount(ListFromRegion(r));
end;

function ListItemCount(lst:GUIList): Longint; overload;
begin
  result := 0;
  if not assigned(lst) then exit;
  
  result := Length(lst^.items);
end;

function ListItemCount(const id : String): Longint; overload;
begin
  result := ListItemCount(RegionWithID(id));
end;

function ListItemCount(pnl : Panel; const id : String): Longint; overload;
begin
  result := ListItemCount(RegionWithID(pnl,id));
end;

procedure ListAddItem(lst: GUIList; const text: String); overload;
begin
  ListAddItem(lst, nil, -1, text);
end;

procedure ListAddItem(lst: GUIList; img:Bitmap); overload;
begin
  ListAddItem(lst, img, -1, '');
end;

procedure ListAddItem(lst: GUIList; img:Bitmap; const text: String); overload;
begin
  ListAddItem(lst, img, -1, text);
end;

procedure ListAddItem(lst: GUIList; img: Bitmap; cell: Longint); overload;
begin
  ListAddItem(lst, img, cell, '');
end;

procedure ListAddItem(lst: GUIList; img: Bitmap; cell: Longint; const text: String); overload;
begin
    if not assigned(lst) then exit;
    
    SetLength(lst^.items, Length(lst^.items) + 1);
    lst^.items[High(lst^.items)].text     := text;  //Assign the text to the item
    lst^.items[High(lst^.items)].image    := img;   //Assign the image to the item
    lst^.items[High(lst^.items)].cell     := cell;  //Assign the image to the item
end;

procedure ListAddItem(r : Region; img: Bitmap; cell: Longint); overload;
begin
  ListAddItem(r,img, cell, '');
end;

procedure ListAddItem(r : Region; img: Bitmap; cell: Longint; const text: String); overload;
begin
  ListAddItem(ListFromRegion(r), img, cell, text);
end;

procedure ListAddItem(r : Region; const text: String); overload;
begin
  ListAddItem(ListFromRegion(r), text);
end;

procedure ListAddItem(r : Region; img:Bitmap); overload;
begin
  ListAddItem(ListFromRegion(r), img);
end;

procedure ListAddItem(r : Region; img:Bitmap; const text: String); overload;
begin
  ListAddItem(ListFromRegion(r),img, text);
end;


procedure ListAddItem(const id : String; img:Bitmap; cell: Longint); overload;
begin
  ListAddItem(RegionWithID(id), img, cell, '');
end;

procedure ListAddItem(const id: String; img: Bitmap; cell: Longint; const text: String); overload;
begin
  ListAddItem(RegionWithID(id), img, cell, text);
end;

procedure ListAddItem(const id, text: String); overload;
begin
  ListAddItem(RegionWithID(id), text);
end;

procedure ListAddItem(const id: String; img:Bitmap); overload;
begin
  ListAddItem(RegionWithID(id), img);
end;

procedure ListAddItem(const id: String; img:Bitmap; const text: String); overload;
begin
  ListAddItem(RegionWithID(id),img, text);
end;

procedure ListAddItem(pnl : Panel; const id : String; img: Bitmap; cell: Longint); overload;
begin
  ListAddItem(RegionWithID(id), img, cell, '');
end;

procedure ListAddItem(pnl : Panel; const id: String; img: Bitmap; cell: Longint; const text: String); overload;
begin
  ListAddItem(RegionWithID(id), img, cell, text);
end;

procedure ListAddItem(pnl : Panel; const id, text: String); overload;
begin
  ListAddItem(RegionWithID(id), text);
end;

procedure ListAddItem(pnl : Panel; const id: String; img:Bitmap); overload;
begin
  ListAddItem(RegionWithID(id), img);
end;

procedure ListAddItem(pnl : Panel; const id: String; img:Bitmap; const text: String); overload;
begin
  ListAddItem(RegionWithID(id),img, text);
end;

procedure ListClearItems(lst: GUIList); overload;
begin
  if not assigned(lst) then exit;
  
  SetLength(lst^.items, 0);
  lst^.activeItem := -1;
  lst^.startingAt := 0;
end;

procedure ListClearItems(r : Region); overload;
begin
  ListClearItems(ListFromRegion(r));
end;

procedure ListClearItems(const id : String); overload;
begin
  ListClearItems(RegionWithID(id));
end;

procedure ListClearItems(pnl : Panel; const id : String); overload;
begin
  ListClearItems(RegionWithID(pnl,id));
end;

procedure ListRemoveActiveItem(r : region); overload;
begin
  ListRemoveItem(ListFromRegion(r), ListActiveItemIndex(r));
end;

procedure ListRemoveActiveItem(const id : string); overload;
begin
  ListRemoveActiveItem(RegionWithID(id));
end;

procedure ListRemoveActiveItem(pnl : Panel; const id : string); overload;
begin
  ListRemoveActiveItem(RegionWithID(pnl, id));
end;



procedure ListRemoveItem(lst: GUIList; idx: Longint);
var
  i: Longint;
begin
    if not assigned(lst) then exit;
    if (idx < 0) or (idx > High(lst^.items)) then exit;
    
    for i := idx to High(lst^.items) - 1 do
    begin
      lst^.items[i] := lst^.items[i + 1];
    end;
    
    SetLength(lst^.items, Length(lst^.items) - 1);
    
    if (lst^.startingAt >= idx) and (idx <> 0) then lst^.startingAt := lst^.startingAt - 1;
    if lst^.activeItem >= idx then lst^.activeItem := lst^.activeItem - 1;
end;


procedure ListRemoveItem(const id : String; idx: Longint);
begin
  ListRemoveItem(ListFromRegion(RegionWithID(id)),idx);
end;

procedure ListRemoveItem(pnl : Panel; const id : String; idx: Longint);
begin
  ListRemoveItem(ListFromRegion(RegionWithID(pnl, id)),idx);
end;

function ListTextIndex(lst: GUIList; const value: String): Longint;
var
  i: Longint;
begin
  result := -1;
  if not assigned(lst) then exit;
  
  for i := Low(lst^.items) to High(lst^.items) do
  begin
    //find the text... then exit
    if lst^.items[i].text = value then
    begin
      result := i;
      exit;
    end;
  end;
end;

function ListBitmapIndex(lst: GUIList; img: Bitmap): Longint; overload;
begin
  result := ListBitmapIndex(lst, img, -1);
end;

function ListBitmapIndex(lst: GUIList; img: Bitmap; cell: Longint): Longint; overload;
var
  i: Longint;
begin
  result := -1;
  if not assigned(lst) then exit;
  
  for i := Low(lst^.items) to High(lst^.items) do
  begin
    //find the text... then exit
    if (lst^.items[i].image = img) and (lst^.items[i].cell = cell) then
    begin
      result := i;
      exit;
    end;
  end;
end;

function ListScrollIncrement(lst: GUIList): Longint;
begin
  result := 1;
  if not assigned(lst) then exit;
  
  if lst^.verticalScroll then
    result := lst^.columns
  else
    result := lst^.rows;
    
  if result <= 0 then result := 1;
end;

function ListActiveItemIndex(r: Region): Longint; overload;
begin
  result := ListActiveItemIndex(ListFromRegion(r));
end;

function ListActiveItemIndex(lst: GUIList): Longint; overload;
begin
  result := -1;
  if not assigned(lst) then exit;
  result := lst^.activeItem;
end;

function ListActiveItemIndex(const id: string): Longint; overload;
begin
  result := ListActiveItemIndex(RegionWithID(id));
end;

function ListActiveItemIndex(pnl: Panel; const id: string): Longint; overload;
begin
  result := ListActiveItemIndex(RegionWithID(pnl, id));
end;

function ListStartAt(lst: GUIList): Longint;
begin
  result := 0;
  if not assigned(lst) then exit;
  
  result := lst^.startingAt;
end;

function ListStartAt(r: Region): Longint;
begin
  result := ListStartAt(ListFromRegion(r));
end;

procedure ListSetStartAt(lst: GUIList; idx: Longint);
begin
  if not assigned(lst) then exit;
  if (idx < 0) then idx := 0
  else if (idx > High(lst^.items)) then idx := (High(lst^.items) div ListScrollIncrement(lst)) * ListScrollIncrement(lst);
  
  lst^.startingAt := idx;
end;

procedure ListSetStartAt(r: Region; idx: Longint);
begin
  ListSetStartAt(ListFromRegion(r), idx);
end;

function ListLargestStartIndex(lst: GUIList): Longint;
var
  placeCount:       Longint;
  itemCount:        Longint;
begin
 result := 0;
 if not assigned(lst) then exit;
   
 itemCount  := Length(lst^.items);
 placeCount := lst^.rows * lst^.columns;
 result     := itemCount - placeCount;
 
 // Round - 1.0 converts this to doubles...
 result := Ceiling(1.0 * result / ListScrollIncrement(lst)) * ListScrollIncrement(lst);
end;

//---------------------------------------------------------------------------------------
// Checkbox Code Code
//---------------------------------------------------------------------------------------

function CheckboxState(chk: GUICheckbox): Boolean; overload;
begin
  if not assigned(chk) then begin result := false; exit; end;
  
  result := chk^.state;
end;

function CheckboxState(r: Region): Boolean; overload;
begin
  result := CheckboxState(CheckboxFromRegion(r));
end;

function CheckboxState(const s: String): Boolean; overload;
begin
  result := CheckboxState(CheckboxFromRegion(RegionWithID(s)));
end;

function CheckboxState(p: Panel; const s: String): Boolean; overload;
begin
  result := CheckboxState(CheckboxFromRegion(RegionWithID(p, s)));
end;

procedure CheckboxSetState(chk: GUICheckbox; val: Boolean); overload;
begin
  if not assigned(chk) then exit;
  
  chk^.state := val;
end;

procedure CheckboxSetState(r: region; val: Boolean); overload;
begin
  CheckboxSetState(CheckboxFromRegion(r), val);
end;

procedure CheckBoxSetState(const id : String; val : Boolean);overload; 
begin
  CheckboxSetState(RegionWithID(id), val);
end;
procedure CheckBoxSetState(pnl: Panel; const id : String; val : Boolean);overload; 
begin
  CheckboxSetState(RegionWithID(pnl, id), val);
end;


// function CheckboxState(const ID: String): Boolean;
// var
//   reg: Region;
// begin
//   reg := GetRegionByID(ID);
//   result := reg^.parent^.checkboxes[reg^.elementIndex].state;
// end;

procedure ToggleCheckboxState(c: GUICheckbox);
begin
  if not assigned(c) then exit;

  c^.state := not c^.state;
end;

procedure ToggleCheckboxState(const id : String);
begin
  ToggleCheckboxState(CheckboxFromRegion(RegionWithID(id)));
end;

procedure ToggleCheckboxState(pnl: Panel; const id : String);
begin
  ToggleCheckboxState(CheckboxFromRegion(RegionWithID(pnl, id)));
end;

//---------------------------------------------------------------------------------------
// Panel Code
//---------------------------------------------------------------------------------------

function PanelDraggable(p: panel): Boolean;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);
  if not assigned(pp) then exit;
  
  Result := pp^.draggable;
end;

procedure PanelSetDraggable(p: panel; b:Boolean);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);
  if not assigned(pp) then exit;
  
   pp^.draggable := b;
end;

procedure PanelSetX(p: Panel; nX: Longint);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if not(assigned(pp)) then exit;
  
  pp^.area.X := nX;
end;

procedure PanelSetY(p: Panel; nY: Longint);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if not(assigned(pp)) then exit;
  
  pp^.area.Y := nY;
end;

function PanelWidth(const name: String): Longint;
begin
    result := PanelWidth(PanelNamed(name));
end;

function PanelWidth(p: Panel): Longint;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  result := -1;
  if not(assigned(pp)) then exit;
  
  result := Round(pp^.area.width);
end;

function PanelHeight(const name: String): Longint;
begin
    result := PanelHeight(PanelNamed(name));
end;

function PanelHeight(p: Panel): Longint;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  result := -1;
  if not(assigned(pp)) then exit;
  
  result := Round(pp^.area.height);
end;

function PanelX(p: Panel): Single;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  result := -1;
  if not(assigned(pp)) then exit;
  
  result := pp^.area.x;
end;

function PanelY(p: Panel): Single;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  result := -1;
  if not(assigned(pp)) then exit;
  
  result := pp^.area.y;
end;

function PanelVisible(p: Panel): Boolean;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  result := false;
  if not(assigned(pp)) then exit;
  
  result := pp^.visible;
end;

procedure AddPanelToGUI(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then
  begin
    SetLength(GUIC.panels, Length(GUIC.panels) + 1);
    GUIC.panels[High(GUIC.panels)] := pp;
  end;
end;

procedure ActivatePanel(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then pp^.active := true;
end;

procedure DeactivatePanel(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then pp^.active := false;
end;

procedure ToggleActivatePanel(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then pp^.active := not pp^.active;
end;

function InitPanel(const name, filename: String): PanelPtr;
begin
  New(result);
  result^.id := PANEL_PTR;
  result^.name      := name;
  result^.filename  := filename;
  
  with result^ do
  begin
    panelID       := -1;
    area          := RectangleFrom(0,0,0,0);
    visible       := false;
    active        := true;      // Panels are active by default - you need to deactivate them specially...
    draggable     := false;
    DrawAsVectors := true;
    modal         := false;
    
    panelBitmap         := nil;
    panelBitmapActive   := nil;
    panelBitmapInactive := nil;
    
    SetLength(regions,      0);
    SetLength(labels,       0);
    SetLength(checkBoxes,   0);
    SetLength(radioGroups,  0);
    SetLength(textBoxes,    0);
    SetLength(lists,        0);
    
    InitNamedIndexCollection(regionIds);   //Setup the name <-> id mappings
  end;
end;

function NewPanel(const pnlName: String): Panel;
var
  pnl: PanelPtr;
  obj: tResourceContainer;
begin
  if GUIC.panelIds.containsKey(pnlName) then
  begin
    result := PanelNamed(pnlName);
    exit;
  end;
  
  pnl := InitPanel(pnlName, 'RuntimePanel');
  obj := tResourceContainer.Create(pnl);
  
  if not GUIC.panelIds.setValue(pnlname, obj) then
  begin
    DoFreePanel(pnl);
    result := nil;
  end
  else
  begin
    AddPanelToGUI(pnl);
    result := Panel(pnl);
  end;
  
end;

procedure ShowPanelDialog(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then 
  begin
    if pp^.visible then //hide it...
      ToggleShowPanel(p);
    
    // show the panel last...
    ToggleShowPanel(p);
    // and make it modal
    pp^.modal := true;
  end;
end;

procedure ShowPanel(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) and (not pp^.visible) then ToggleShowPanel(p);
end;

procedure ShowPanel(const name: String);
begin
  ShowPanel(PanelNamed(name));
end;

procedure HidePanel(p: Panel);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) and (pp^.visible) then ToggleShowPanel(p);
end;

procedure HidePanel(const name: String);
begin
  HidePanel(PanelNamed(name));
end;


procedure ToggleShowPanel(p: Panel);
var
  i: Longint;
  found: Boolean;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);

  if assigned(pp) then
  begin
    pp^.visible  := not pp^.visible;
    
    // set to non-modal by default
    pp^.modal    := false;
    
    if pp^.visible then
    begin
      // Now visible so add to the visible panels
      SetLength(GUIC.visiblePanels, Length(GUIC.visiblePanels) + 1);
      GUIC.visiblePanels[High(GUIC.visiblePanels)] := pp;
    end
    else
    begin
      // No longer visible - remove from visible panels
      found := false;
      
      // Search for panel and remove from visible panels
      for i := 0 to High(GUIC.visiblePanels) do
      begin
        if (not found) then
        begin
          if (p = GUIC.visiblePanels[i]) then found := true;
        end
        else // found - so copy over...
          GUIC.visiblePanels[i - 1] := GUIC.visiblePanels[i]
      end;
      
      SetLength(GUIC.visiblePanels, Length(GUIC.visiblePanels) - 1);
    end;
  end;
end;

function MousePointInPanel(pnl: Panel): Point2D;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(pnl);

  result.x := -1; result.y := -1;
  if not assigned(pp) then exit;
  
  result := PointAdd(MousePosition(), InvertVector(RectangleTopLeft(pp^.area)));
end;

function PanelClicked(): Panel;
begin
  result := nil;
  
  if not ModalBefore(GUIC.panelClicked) then result := GUIC.panelClicked;
end;

function PanelClicked(pnl: Panel): Boolean;
begin
  if not(assigned(pnl)) then exit;
  if not(assigned(GUIC.panelClicked)) then exit;

  result := GUIC.panelClicked = pnl;
end;

//=============================================================================
// Create Panels/GUI Elements/Regions etc.
//=============================================================================

function StringToKind(const s: String): GUIElementKind;
  begin
    if Lowercase(s) = 'button' then
      result := gkButton
    else if Lowercase(s) = 'label' then
      result := gkLabel
    else if Lowercase(s) = 'textbox' then
      result := gkTextBox
    else if Lowercase(s) = 'checkbox' then
      result := gkCheckBox
    else if Lowercase(s) = 'radiogroup' then
      result := gkRadioGroup
    else if Lowercase(s) = 'list' then
      result := gkList
    else 
      RaiseException(s + ' is an invalid kind for region.');
  end;
  
  procedure CreateLabel(forRegion: RegionPtr; const d: string; result: PanelPtr);
  var
    newLbl: GUILabelData;
  begin
    //Format is
    // 1, 2, 3, 4, 5, 6,   7,    8,     9, 
    // x, y, w, h, 5, id , font, align, text
    forRegion^.font       := FontNamed(Trim(ExtractDelimited(7, d, [','])));
    forRegion^.alignment  := TextAlignmentFrom(Trim(ExtractDelimited(8, d, [','])));
    newLbl.contentString  := Trim(ExtractDelimited(9, d, [',']));
    
    SetLength(result^.Labels, Length(result^.Labels) + 1);
    result^.labels[High(result^.labels)] := newLbl;
    forRegion^.elementIndex := High(result^.labels);  // The label index for the region -> so it knows which label
  end;
  
  procedure AddRegionToGroup(regToAdd: RegionPtr; groupToRecieve: GUIRadioGroup);
  begin
    SetLength(groupToRecieve^.buttons, Length(groupToRecieve^.buttons) + 1);
    groupToRecieve^.buttons[High(groupToRecieve^.buttons)] := regToAdd;    
  end;
  
  procedure CreateRadioButton(forRegion: RegionPtr; const data: String; result: PanelPtr);
  var
    newRadioGroup: GUIRadioGroupData;
    i: Longint;
    radioGroupID: string;
  begin
    radioGroupID := Trim(ExtractDelimited(7,data,[',']));
    
    for i := Low(result^.radioGroups) to High(result^.radioGroups) do
    begin
      if (radioGroupID = result^.radioGroups[i].GroupID) then
      begin
        AddRegionToGroup(forRegion, @result^.radioGroups[i]);
        forRegion^.elementIndex := i;
        exit;
      end;
    end;
    
    SetLength(newRadioGroup.buttons, 0);
    newRadioGroup.GroupID := radioGroupID;
    AddRegionToGroup(forRegion, @newRadioGroup);
    
    newRadioGroup.activeButton := 0;
    // add to panel, record element index.
    SetLength(result^.radioGroups, Length(result^.radioGroups) + 1);
    result^.radioGroups[High(result^.radioGroups)] := newRadioGroup;
    forRegion^.elementIndex := High(result^.radioGroups);
  end;
  
  procedure CreateCheckbox(forRegion: RegionPtr; const data: string; result: PanelPtr);
  begin
    SetLength(result^.Checkboxes, Length(result^.Checkboxes) + 1);
    result^.Checkboxes[High(result^.Checkboxes)].state := LowerCase(ExtractDelimited(7, data, [','])) = 'true';
    forRegion^.elementIndex := High(result^.Checkboxes);
  end;
  
  procedure CreateTextbox(r: RegionPtr; const data: String; result: PanelPtr);
  var
    newTextbox: GUITextboxData;
  begin
    //Format is
    // 1, 2, 3, 4, 5, 6,   7,    8,      9,     10, 
    // x, y, w, h, 5, id , font, maxLen, align, text
    
    if CountDelimiter(data, ',') <> 9 then
    begin
      RaiseException('Error with textbox (' + data + ') should have 10 values (x, y, w, h, 5, id, font, maxLen, align, text)');
      exit;
    end;
    
    r^.font                   := FontNamed(Trim(ExtractDelimited(7, data, [','])));
    newTextbox.lengthLimit    := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    r^.alignment              := TextAlignmentFrom(Trim(ExtractDelimited(9, data, [','])));
    newTextBox.contentString  := Trim(ExtractDelimited(10, data, [',']));
    newTextBox.forRegion      := r;
    
    SetLength(result^.textBoxes, Length(result^.textBoxes) + 1);
    result^.textBoxes[High(result^.textBoxes)] := newTextbox;
    r^.ElementIndex := High(result^.textBoxes);
  end;
  
  procedure CreateList(r: RegionPtr; const data: string; result: PanelPtr);
  var
    newList: GUIListData;
    scrollSz, rhs, btm, height, width: Single;
  begin
    //Format is
    // 1, 2, 3, 4, 5, 6,      7,       8,    9,          10,     11,        12,         13          14                  
    // x, y, w, h, 5, ListID, Columns, Rows, ActiveItem, fontID, alignment, scrollSize, scrollKind, scrollbutton bitmap
    
    newList.columns         := StrToInt(Trim(ExtractDelimited(7, data, [','])));
    newList.rows            := StrToInt(Trim(ExtractDelimited(8, data, [','])));
    newList.activeItem      := StrToInt(Trim(ExtractDelimited(9, data, [','])));
    r^.font                 := FontNamed(Trim(ExtractDelimited(10, data, [','])));
    r^.alignment            := TextAlignmentFrom(Trim(ExtractDelimited(11, data, [','])));
    newList.scrollSize      := StrToInt(Trim(ExtractDelimited(12, data, [','])));;
    newList.verticalScroll  := LowerCase(Trim(ExtractDelimited(13, data, [',']))) = 'v';
    
    if Trim(ExtractDelimited(14, data, [','])) <> 'n' then
    begin
      //LoadBitmap(Trim(ExtractDelimited(13, data, [','])));
      newList.scrollButton    := LoadBitmap(Trim(ExtractDelimited(14, data, [',']))); //BitmapNamed(Trim(ExtractDelimited(13, data, [','])));
    end
    else
      newList.scrollButton    := nil;
    
    scrollSz := newList.scrollSize;
    
    // Start at the fist item in the list, or the activeItem
    if newList.activeItem = -1 then
      newList.startingAt    := 0
    else
      newList.startingAt    := newList.activeItem;
    
    rhs     := r^.area.width - scrollSz;
    btm     := r^.area.height - scrollSz;
    height  := r^.area.height;
    width   := r^.area.width;
    
    // Calculate col and row sizes
    if newList.verticalScroll then
    begin
      newList.colWidth    := rhs    / newList.columns;
      newList.rowHeight   := height / newList.rows;
      
      // Set scroll buttons
      newList.scrollUp    := RectangleFrom( rhs, 0, scrollSz, scrollSz);
      newList.scrollDown  := RectangleFrom( rhs, btm, scrollSz, scrollSz);
      
      newList.scrollArea  := RectangleFrom( rhs, scrollSz, scrollSz, height - (2 * scrollSz));
    end
    else
    begin
      newList.colWidth    := r^.area.width / newList.columns;
      newList.rowHeight   := btm / newList.rows;
      
      // Set scroll buttons
      newList.scrollUp    := RectangleFrom( 0, btm, scrollSz, scrollSz);
      newList.scrollDown  := RectangleFrom( rhs, btm, scrollSz, scrollSz);
      
      newList.scrollArea  := RectangleFrom( scrollSz, btm, width - (2 * scrollSz), scrollSz);
    end;
    
    
    SetLength(newList.placeHolder, (newList.columns * newList.rows));
    
    SetLength(newList.items, 0);
    
    SetLength(result^.lists, Length(result^.lists) + 1);
    result^.lists[High(result^.lists)] := newList;
    r^.elementIndex := High(result^.lists);
  end;
  
  type RegionDataArray = Array of RegionData;
  
  // Rewires points to regions in the panel, assumes that the
  // newRegions is larger than or equal to the number of old regions in the panel
  procedure RewireRegions(p: PanelPtr; newRegions: RegionDataArray);
  var
    i, j, elemIdx: Longint;
  begin
    
    // for all of the regions
    for i := 0 to High(p^.regions) do
    begin
      //Copy the old region data to the new region data
      newRegions[i] := p^.regions[i];
      //Get the element index
      elemIdx := p^.regions[i].elementIndex;
      
      // if a textbox or a radio group, remap pointers
      case p^.regions[i].kind of
        gkTextbox: p^.textBoxes[elemIdx].forRegion := @newRegions[i];
        gkRadioGroup:
        begin
          for j := 0 to High(p^.radioGroups[elemIdx].buttons) do
          begin
            //searching for the button that points to the old regions
            if p^.radioGroups[elemIdx].buttons[j] = @p^.regions[i] then
            begin 
               p^.radioGroups[elemIdx].buttons[j] := @newRegions[i];
               break; // exit inner loop
            end;
          end;
        end;
      end; // end case
    end;// end for
    
    // move the new regions in (copies pointer)
    p^.regions := newRegions;    
  end;
  
  procedure AddRegionToPanelWithString(const d: string; p: PanelPtr);
  var
    regID: string;
    regX, regY: Single;
    regW, regH, addedIdx: LongInt;
    r: RegionData;
    newRegions: RegionDataArray;
  begin
    
    // Format is 
    // x, y, w, h, kind, id
    regX := Single(StrToFloat(Trim(ExtractDelimited(1, d, [',']))));
    regY := Single(StrToFloat(Trim(ExtractDelimited(2, d, [',']))));
    regW := StrToInt(Trim(ExtractDelimited(3, d, [','])));
    regH := StrToInt(Trim(ExtractDelimited(4, d, [','])));
    
    r.id := REGION_PTR;
    r.kind := StringToKind(Trim(ExtractDelimited(5, d, [','])));
    
    regID := Trim(ExtractDelimited(6, d, [',']));
    
    // this should be done when used... so that moving the panel effects this
    //
    // regX += p^.area.x;
    // regY += p^.area.y;
    
    
        
    addedIdx := AddName(p^.regionIds, regID);   //Allocate the index
      
    if High(p^.Regions) < addedIdx then 
    begin 
      
      // Make more space and rewire region pointers
      SetLength(newRegions, addedIdx + 1);
      
      RewireRegions(p, newRegions);
      
    end;
    
    r.regionIdx     := addedIdx;
    r.area          := RectangleFrom(regX, regY, regW, regH);
    r.active        := true;
    r.stringID      := regID;
    r.elementIndex  := -1;
    r.parent        := p;
    SetLength(r.callbacks, 0);
    
    p^.Regions[addedIdx] := r;
    
    case r.kind of
      gkButton:     ;
      gkLabel:      CreateLabel(@p^.Regions[addedIdx],d,p);
      gkCheckbox:   CreateCheckbox(@p^.Regions[addedIdx],d,p);
      gkRadioGroup: CreateRadioButton(@p^.Regions[addedIdx],d,p);
      gkTextbox:    CreateTextbox(@p^.Regions[addedIdx],d,p);
      gkList:       CreateList(@p^.Regions[addedIdx], d,p);
    end;    
  end;
  
function DoLoadPanel(const filename, name: string): PanelPtr;
var
  pathToFile, line, id, data: String;
  panelFile: Text;
  lineNo: LongInt;
  regionDataArr: Array of String;
  
  procedure StoreRegionData(const data: String);
  begin
    SetLength(regionDataArr, Length(regionDataArr) + 1);
    regionDataArr[High(regionDataArr)] := data;
  end;
  
  procedure ProcessLine();
  begin
    // Split line into id and data around :
    id := ExtractDelimited(1, line, [':']); // where id comes before...
    data := ExtractDelimited(2, line, [':']); // ...and data comes after.
  
    // Verify that id is a single char
    if Length(id) <> 1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be a single character.');
      exit; // Fail
    end;
    // Process based on id
    case LowerCase(id)[1] of // in all cases the data variable is read
      'x': result^.area.x       := MyStrToInt(data, false);
      'y': result^.area.y       := MyStrToInt(data, false);
      'w': result^.area.width   := MyStrToInt(data, false);
      'h': result^.area.height  := MyStrToInt(data, false);
      'b': begin
             LoadBitmap(Trim(data)); 
             result^.panelBitmap := BitmapNamed(Trim(data));
           end;
      'a': begin
             LoadBitmap(Trim(data));
             result^.panelBitmapActive := BitmapNamed(Trim(data));
           end;
      'i': begin
             LoadBitmap(Trim(data));
             result^.panelBitmapInactive := BitmapNamed(Trim(data));
           end;
      'r': StoreRegionData(data);
      'd': result^.draggable     := (LowerCase(Trim(data)) = 'true');
      'v': result^.DrawAsVectors := (LowerCase(Trim(data)) = 'true');
    else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in panel: ' + filename + '. Error with id: ' + id + '. This should be one of the characters defined in the template.');
        exit;
      end;
    end;
  end;
  
  procedure CreateRegions();
  var
    i: Longint;
  begin
    SetLength(result^.regions, Length(regionDataArr));
    
    for i := Low(regionDataArr) to High(regionDataArr) do
    begin
      AddRegionToPanelWithString(regionDataArr[i], result);
    end;
  end;
  
  procedure SetupListPlaceholders();
  var
    tempListPtr: GUIList;
    workingCol, workingRow: Longint;
    j, k: Longint;
    
  begin
    for j := Low(result^.Regions) to High(result^.Regions) do
    begin
      //Get the region as a list (will be nil if not list...)
      tempListPtr := ListFromRegion(@result^.regions[j]);
      
      if assigned(tempListPtr) then
      begin
        workingCol := 0;
        workingRow := 0;
        
        for k := Low(tempListPtr^.placeHolder) to High(tempListPtr^.placeHolder) do
        begin
          tempListPtr^.placeHolder[k].x := (workingCol * tempListPtr^.colWidth);
          tempListPtr^.placeHolder[k].y := (workingRow * tempListPtr^.rowHeight);
          
          tempListPtr^.placeHolder[k].width := (tempListPtr^.colWidth);
          tempListPtr^.placeHolder[k].height := (tempListPtr^.rowHeight);
                    
          workingCol += 1;
          
          if workingCol >= tempListPtr^.columns then
          begin
            workingCol := 0;
            workingRow += 1;
          end;
        end;            
      end;
    end;
  end;
  
begin
  pathToFile := filename;
  
  if not FileExists(pathToFile) then
  begin
    pathToFile := PathToResource(filename, PanelResource);
    if not FileExists(pathToFile) then
    begin
      RaiseWarning('Unable to locate panel ' + filename + ' at ' + pathToFile);
      result := nil;
      exit;
    end;
  end;
  
  // Initialise the resulting panel
  result := InitPanel(name, filename);
  
  Assign(panelFile, pathToFile);
  Reset(panelFile);
  
  try  
    SetLength(regionDataArr, 0);
  
    lineNo := -1;
  
    while not EOF(panelFile) do
    begin
      lineNo := lineNo + 1;
    
      ReadLn(panelFile, line);
      line := Trim(line);
      if Length(line) = 0 then continue;  //skip empty lines
      if MidStr(line,1,2) = '//' then continue; //skip lines starting with // - the first character at index 0 is the length of the string.
  
      ProcessLine();
    end;
  
    CreateRegions();
    SetupListPlaceholders();
  finally
    Close(panelFile);
  end;
end;

procedure GUISetForegroundColor(c:Color);
begin
  GUIC.foregroundClr := c;
end;

procedure GUISetForegroundColorInactive(c:color);
begin
  GUIC.foregroundClrInactive := c;
end;

procedure GUISetBackgroundColorInactive(c:color);
begin
  GUIC.backgroundClrInactive := c;
end;
procedure GUISetBackgroundColor(c:Color);
begin
  GUIC.backgroundClr := c;
end;

procedure DrawGUIAsVectors(b : Boolean);
begin
  GUIC.VectorDrawing := b;
end;

function PanelActive(pnl: Panel): Boolean;
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(pnl);
  if assigned(pp) then 
    result := pp^.active and not ModalBefore(pnl)
  else result := false;
end;

function IsSet(toCheck, checkFor: GUIElementKind): Boolean; overload;
begin
  result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
end;

function PointInRegion(const pt: Point2D; p: Panel; kind: GUIElementKind): Boolean; overload;
var
  i: Longint;
  curr: RegionPtr;
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);
  result := false;
  if not assigned(pp) then exit;
  
  for i := Low(pp^.Regions) to High(pp^.Regions) do
  begin
    curr := @pp^.Regions[i];
    if PointInRect(MousePointInPanel(p), curr^.area) and IsSet(kind, curr^.kind) then 
    begin     
      result := true;
      exit;
    end;
  end;
end;

function PointInRegion(const pt: Point2D; p: Panel): Boolean; overload;
begin
  result := PointInRegion(pt, p, gkAnyKind);
end;

function PanelAtPoint(const pt: Point2D): Panel;
var
  i: Longint;
  curPanel: PanelPtr;
begin
  result := nil;
  
  for i := High(GUIC.visiblePanels) downto Low(GUIC.visiblePanels) do
  begin
    curPanel := GUIC.visiblePanels[i];
    
    if PointInRect(pt, GUIC.visiblePanels[i]^.area) then
    begin
      result := curPanel;
      exit;
    end;
  end;
end;

procedure DragPanel();
begin
  if not IsDragging() then exit;
  
  if IsDragging() then
  begin
    MovePanel(GUIC.panelDragging, MouseMovement());
  end;
end;

function IsDragging(): Boolean;
begin
  result := assigned(GUIC.panelDragging) and GUIC.panelDragging^.draggable;
end;

function IsDragging(pnl: Panel): Boolean;
begin
  result := false;
  if not(assigned(pnl)) then exit;
  if not(assigned(GUIC.panelDragging)) then exit;
  
  result := GUIC.panelDragging = pnl;
end;

procedure StartDragging();
var
  mp: Point2D;
  curPanel: Panel;
begin
  mp := MousePosition();
  curPanel := PanelAtPoint(mp);
  
  if not PointInRegion(mp, curPanel, GUIElementKind(Longint(gkAnyKind) and not Longint(gkLabel))) then
  begin
    GUIC.panelDragging := curPanel;
  end
  else
  begin
    GUIC.panelDragging := nil;
  end;
end;

procedure StopDragging();
begin
  GUIC.panelDragging  := nil;
  GUIC.downRegistered := false;
end;

procedure MovePanel(p: Panel; const mvmt: Vector);
var
  pp: PanelPtr;
begin
  pp := ToPanelPtr(p);
  if not assigned(pp) then exit;
  
  pp^.area := RectangleOffset(pp^.area, mvmt);
end;

function ButtonClicked(r: Region) : Boolean;
begin
  result := (RegionClicked() = r) and (r <> nil);
end;

function ButtonClicked(const name: String) : Boolean;
begin
  result := ButtonClicked(RegionWithId(name));
end;

// function DoLoadPanel(const filename, name: String): Panel;
// begin
//   {$IFDEF TRACE}
//     TraceEnter('sgUserInterface', 'DoLoadPanel', name + ' = ' + filename);
//   {$ENDIF}
//   
//   if not FileExists(filename) then
//   begin
//     filename := PathToResource(filename, PanelResource);
//     if not FileExists(filename) then
//     begin
//       RaiseException('Unable to locate panel ' + filename);
//       exit;
//     end;
//   end;
//   
//   New(result);    
//   result^.effect := Mix_LoadWAV(PChar(filename));
//   result^.filename := filename;
//   result^.name := name;
//   
//   if result^.effect = nil then
//   begin
//     Dispose(result);
//     RaiseException('Error loading panel: ' + MIX_GetError());
//     exit;
//   end;
//   
//   {$IFDEF TRACE}
//     TraceExit('sgUserInterface', 'DoLoadPanel', HexStr(result));
//   {$ENDIF}
// end;

function LoadPanel(const filename: String): Panel;
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'LoadPanel', filename);
  {$ENDIF}
  
  result := LoadPanelNamed(filename, filename);
  
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'LoadPanel');
  {$ENDIF}
end;


function LoadPanelNamed(const name, filename: String): Panel;
var
  obj: tResourceContainer;
  pnl: Panel;
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'MapPanel', name + ' = ' + filename);
  {$ENDIF}
  
  if GUIC.panelIds.containsKey(name) then
  begin
    result := PanelNamed(name);
    exit;
  end;
  
  pnl := DoLoadPanel(filename, name);
  if not assigned(pnl) then
  begin
    result := nil;
    exit;
  end;

  obj := tResourceContainer.Create(pnl);
  
  if not GUIC.panelIds.setValue(name, obj) then
  begin
    DoFreePanel(pnl);
    result := nil;
  end
  else
  begin
    AddPanelToGUI(pnl);
    result := pnl;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'MapPanel');
  {$ENDIF}
end;

// private:
// Called to actually free the resource

procedure DoFreePanel(var pnl: Panel);
var 
  i, j: Longint;
  pp: PanelPtr;
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'DoFreePanel', 'pnl = ' + HexStr(pnl));
  {$ENDIF}
  
  pp := ToPanelPtr(pnl);

  if assigned(pp) then
  begin
    
    if GUIC.panelDragging = pp then GUIC.panelDragging := nil;
    if dialog.dialogPanel = pp then dialog.dialogPanel := nil;
    
    HidePanel(pp);
    
    for i := Low(GUIC.panels) to High(GUIC.panels) do
    begin
      if GUIC.panels[i] = pp then
      begin
        for j := i to (High(GUIC.panels) - 1) do
        begin
          GUIC.panels[j] := GUIC.panels[j + 1]
        end;
        
        SetLength(GUIC.panels, Length(GUIC.panels) - 1);
        break;
      end;
    end;
    
    for i:= Low(pp^.labels) to High(pp^.labels) do
    begin
      CallFreeNotifier(@pp^.labels[i]);
    end;
    
    for i:= Low(pp^.checkBoxes) to High(pp^.checkBoxes) do
    begin
      CallFreeNotifier(@pp^.checkBoxes[i]);
    end;
    
    for i:= Low(pp^.radioGroups) to High(pp^.radioGroups) do
    begin
      CallFreeNotifier(@pp^.radioGroups[i]);
    end;
    
    for i:= Low(pp^.textBoxes) to High(pp^.textBoxes) do
    begin
      CallFreeNotifier(@pp^.textBoxes[i]);
    end;

    for i:= Low(pp^.lists) to High(pp^.lists) do
    begin
      CallFreeNotifier(@pp^.lists[i]);
    end;
    
    for i:= Low(pp^.regions) to High(pp^.regions) do
    begin
      CallFreeNotifier(@pp^.regions[i]);
    end;
    
    CallFreeNotifier(pnl);
    
    FreeNamedIndexCollection(pp^.regionIds);
    Dispose(pp);
  end;
  
  pnl := nil;
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'DoFreePanel');
  {$ENDIF}
end;

procedure FreePanel(var pnl: Panel);
var
  pp: PanelPtr;
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'FreePanel', 'pnl = ' + HexStr(pnl));
  {$ENDIF}
  
  pp := ToPanelPtr(pnl);

  if(assigned(pp)) then
  begin
    ReleasePanel(pp^.name);
  end;
  pnl := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'FreePanel');
  {$ENDIF}
end;

procedure ReleasePanel(const name: String);
var
  pnl: PanelPtr;
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'ReleasePanel', 'pnl = ' + name);
  {$ENDIF}
  
  pnl := ToPanelPtr(PanelNamed(name));
  if (assigned(pnl)) then
  begin
    GUIC.panelIds.remove(name).Free();
    DoFreePanel(pnl);
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'ReleasePanel');
  {$ENDIF}
end;

procedure ReleaseAllPanels();
begin
  {$IFDEF TRACE}
    TraceEnter('sgUserInterface', 'ReleaseAllPanels', '');
  {$ENDIF}
  
  ReleaseAll(GUIC.panelIds, @ReleasePanel);
  
  {$IFDEF TRACE}
    TraceExit('sgUserInterface', 'ReleaseAllPanels');
  {$ENDIF}
end;

function HasPanel(const name: String): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'HasPanel', name);
    {$ENDIF}
    
    result := GUIC.panelIds.containsKey(name);
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'HasPanel', BoolToStr(result, true));
    {$ENDIF}
  end;

  function PanelNamed(const name: String): Panel;
  var
    tmp : TObject;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'PanelNamed', name);
    {$ENDIF}
    
    tmp := GUIC.panelIds.values[name];
    if assigned(tmp) then result := Panel(tResourceContainer(tmp).Resource)
    else result := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'PanelNamed', HexStr(result));
    {$ENDIF}
  end;
  
  function PanelName(pnl: Panel): String;
  var
    pp: PanelPtr;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'PanelName', HexStr(pnl));
    {$ENDIF}

    pp := ToPanelPtr(pnl);
    
    if assigned(pp) then result := pp^.name
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'PanelName', result);
    {$ENDIF}
  end;
  
  function PanelFilename(pnl: Panel): String;
  var
    pp: PanelPtr;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'PanelFilename', HexStr(pnl));
    {$ENDIF}

    pp := ToPanelPtr(pnl);
    
    if assigned(pp) then result := pp^.filename
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'PanelFilename', result);
    {$ENDIF}
  end;

procedure UpdateDrag();
begin
  // if mouse is down and this is the first time we see it go down
  if MouseDown(LeftButton) and not GUIC.downRegistered then
  begin
    GUIC.downRegistered := true;
    
    if ReadingText() then
      FinishReadingText();
    
    StartDragging();
  end
  // if mouse is now up... and it was down before...
  else if MouseUp(LeftButton) and GUIC.downRegistered then
  begin
    StopDragging();
  end;
  
  // If it is dragging then...
  if IsDragging() then
    DragPanel();
end;

// Dialog code
procedure InitialiseFileDialog();
begin
  if not HasPanel('fdFileDialog') then LoadResourceBundle('FileDialog.txt');
  
  with dialog do
  begin
    dialogPanel := PanelNamed('fdFileDialog');
    
    lastSelectedFile := -1;
    lastSelectedPath := -1;
    
    cancelled     := false;
  end;
end;

procedure ShowOpenDialog(select: FileDialogSelectType); overload;
begin
  InitialiseFileDialog();
  
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OkLabel', 'Open');
    LabelSetText(dialogPanel, 'FileEntryLabel', 'Open');
    
    allowNew      := false;
    selectType    := select;
    ShowPanelDialog(dialogPanel);
    
    if length(currentPath) = 0 then 
      DialogSetPath(GetUserDir());
  end;
end;

procedure ShowOpenDialog(); overload;
begin
  ShowOpenDialog(fdFilesAndDirectories);
end;

procedure ShowSaveDialog(select: FileDialogSelectType); overload;
begin
  InitialiseFileDialog();
  
  with dialog do
  begin
    LabelSetText(dialogPanel, 'OkLabel', 'Save');
    LabelSetText(dialogPanel, 'FileEntryLabel', 'Save');
    
    allowNew      := true;
    selectType    := select;
    ShowPanelDialog(dialogPanel);
    
    if length(currentPath) = 0 then 
      DialogSetPath(GetUserDir());
  end;
end;

procedure ShowSaveDialog(); overload;
begin
  ShowSaveDialog(fdFilesAndDirectories);
end;

function DialogComplete(): Boolean;
begin
  result := (dialog.complete) and (Not(dialog.cancelled));
end;

function DialogCancelled(): Boolean;
begin
  result := dialog.cancelled;
end;

function DialogPath(): String;
begin
  if dialog.cancelled then result := ''
  else result := ExpandFileName(dialog.currentSelectedPath);
end;

procedure UpdateDialogPathText();
var
  selectedPath: String;
  pathTxt: Region;
begin
  selectedPath  := dialog.currentSelectedPath;
  
  pathTxt := RegionWithId(dialog.dialogPanel,'PathTextbox');
  
  // WriteLn('tw: ', TextWidth(TextboxFont(pathTxt), selectedPath));
  // WriteLn('rw: ', RegionWidth(pathTxt));
  if TextWidth(RegionFont(pathTxt), selectedPath) > RegionWidth(pathTxt) then
    RegionSetFontAlignment(pathTxt, AlignRight)
  else 
    RegionSetFontAlignment(pathTxt, AlignLeft);
  
  TextboxSetText(pathTxt, selectedPath);
end;

function IsSet(toCheck, checkFor: FileDialogSelectType): Boolean; overload;
begin
  result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
end;

procedure ShowErrorMessage(const msg: String);
begin
  LabelSetText(dialog.dialogPanel, 'ErrorLabel', msg);
  PlaySoundEffect(SoundEffectNamed('fdDialogError'), 0.5);
end;

procedure DialogSetPath(const fullname: String);
var
  tmpPath, path, filename: String;
  paths: Array [0..255] of PChar;
  
  procedure _PopulatePathList();
  var
    pathList: GUIList;
    i, len: Longint;
  begin
    pathList  := ListFromRegion(RegionWithId(dialog.dialogPanel, 'PathList'));
    
    // Clear the list of all of its items
    ListClearItems(pathList);
    
    // Add the drive letter at the start
    if Length(ExtractFileDrive(path)) = 0 then
      ListAddItem(pathList, PathDelim)
    else
      ListAddItem(pathList, ExtractFileDrive(path));
    
    // Set the details in the path list
    len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
    
    // Loop through the directories adding them to the list
    for i := 0 to len - 1 do
    begin
      ListAddItem(pathList, paths[i]);
    end;
    
    // Ensure that the last path is visible in the list
    ListSetStartAt(pathList, ListLargestStartIndex(pathList));
  end;
  
  procedure _PopulateFileList();
  var
    filesList: GUIList;
    info : TSearchRec;
  begin
    filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
    
    // Clear the files in the filesList
    ListClearItems(filesList);
    
    //Loop through the directories
    if FindFirst (dialog.currentPath + '*', faAnyFile and faDirectory, info) = 0 then
    begin
      repeat
        with Info do
        begin
          if (attr and faDirectory) = faDirectory then 
          begin
            // its a directory... always add it
            ListAddItem(filesList, BitmapNamed('fdFolder'), name);
          end
          else
          begin
            // Its a file ... add if not dir only
            if IsSet(dialog.selectType, fdFiles) then
            begin
              ListAddItem(filesList, BitmapNamed('fdFile'), name);
            end;
          end;
        end;
      until FindNext(info) <> 0;
    end;
    FindClose(info);
  end;
  
  procedure _SelectFileInList();
  var
    filesList: GUIList;
    fileIdx: Longint;
  begin
    filesList := ListFromRegion(RegionWithId(dialog.dialogPanel, 'FilesList'));
    if Length(filename) > 0 then
    begin
      fileIdx := ListTextIndex(filesList, filename);
      ListSetActiveItemIndex(filesList, fileIdx);
      ListSetStartAt(filesList, fileIdx);
    end;
  end;
begin
  // expand the relative path to absolute
  if not ExtractFileAndPath(fullname, path, filename, dialog.allowNew) then 
  begin
    ShowErrorMessage('Unable to find file or path.');
    exit;
  end;
  
  // Get the path without the ending delimiter (if one exists...)
  tmpPath := ExcludeTrailingPathDelimiter(path);
  
  with dialog do
  begin
    currentPath         := IncludeTrailingPathDelimiter(tmpPath);
    currentSelectedPath := path + filename;
    lastSelectedPath    := -1;
    lastSelectedFile    := -1;
  end;
  
  UpdateDialogPathText();
  
  _PopulatePathList();
  _PopulateFileList();
  _SelectFileInList();
end;

procedure DialogCheckComplete();
var
  path: String;
begin
  path := DialogPath();
  
  if (not dialog.allowNew) and (not FileExists(path)) then
  begin
    ShowErrorMessage('Select an existing file or path.');
    exit;
  end;
  
  // The file exists, but is not a directory and files cannot be selected
  if (not IsSet(dialog.selectType, fdFiles)) and FileExists(path) and (not DirectoryExists(path)) then
  begin
    ShowErrorMessage('Please select a directory.');
    exit;
  end;
  
  if (not IsSet(dialog.selectType, fdDirectories)) and DirectoryExists(path) then
  begin
    ShowErrorMessage('Please select a file.');
    exit;
  end;
  Dialog.Complete := true;
  HidePanel(dialog.dialogPanel);
end;

procedure UpdateFileDialog();
var
  clicked: Region;
  
  procedure _PerformFileListClick();
  var
    selectedText, selectedPath: String;
    selectedIdx: Longint;
  begin
    // Get the idx of the item selected in the files list
    selectedIdx := ListActiveItemIndex(clicked);
    
    if (selectedIdx >= 0) and (selectedIdx < ListItemCount(clicked)) then
    begin
      selectedText := ListItemText(clicked, selectedIdx);
      selectedPath := dialog.currentPath + selectedText;
    end
    else exit;
    
    // if it is double click...
    if (dialog.lastSelectedFile = selectedIdx) then
    begin
      if DirectoryExists(selectedPath) then
        DialogSetPath(selectedPath)
      else
        DialogCheckComplete();
    end
    else
    begin
      // Update the selected file for double click
      dialog.lastSelectedFile     := selectedIdx;
      
      // Update the selected path and its label
      dialog.currentSelectedPath  := selectedPath;
      UpdateDialogPathText();
    end;
  end;
  
  procedure _PerformPathListClick();
  var
    tmpPath, newPath: String;
    selectedIdx, i, len: Longint;
    paths: Array [0..256] of PChar;
  begin
    // Get the idx of the item selected in the paths
    selectedIdx := ListActiveItemIndex(clicked);
    
    // if it is double click...
    if (dialog.lastSelectedPath = selectedIdx) and (selectedIdx >= 0) and (selectedIdx < ListItemCount(clicked)) then
    begin
      // Get the parts of the current path, and reconstruct up to (and including) selectedIdx
      tmpPath := dialog.currentPath;
      len := GetDirs(tmpPath, paths); //NOTE: need to use tmpPath as this strips separators from pass in string...
      
      newPath := ExtractFileDrive(dialog.currentPath) + PathDelim;
      for i := 0 to Min(selectedIdx - 1, len) do
      begin
        //Need to exclude trailing path delimiter as some paths will include this at the end...
        newPath := newPath + ExcludeTrailingPathDelimiter(paths[i]) + PathDelim;
      end;
      
      DialogSetPath(newPath);
    end
    else
      dialog.lastSelectedPath := selectedIdx;
  end;
  
  procedure _PerformCancelClick();
  begin
    HidePanel(dialog.dialogPanel);
    dialog.cancelled := true;
  end;
  
  procedure _PerformOkClick();
  begin
    DialogCheckComplete();
  end;
  
  procedure _PerformChangePath();
  var
    newPath: String;
  begin
    newPath := TextboxText(RegionWithID(dialog.dialogPanel, 'PathTextbox'));
    
    DialogSetPath(newPath);
  end;
begin
  if PanelClicked() = dialog.dialogPanel then
  begin
    clicked := RegionClicked();
    
    if clicked = RegionWithID(dialog.dialogPanel, 'FilesList') then
      _PerformFileListClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'PathList') then
      _PerformPathListClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'CancelButton') then
      _PerformCancelClick()
    else if clicked = RegionWithID(dialog.dialogPanel, 'OkButton') then
      _PerformOkClick();
  end;
  
  if GUITextEntryComplete() and (RegionOfLastUpdatedTextBox() = RegionWithID(dialog.dialogPanel, 'PathTextbox')) then
  begin
    _PerformChangePath();
  end;
end;

procedure UpdateInterface();
  procedure _UpdateTextEntry();
  var
    pnl: PanelPtr;
    r: RegionPtr;
    txtCount, inc, idx: Longint;
    txt: GUITextBox;
  begin
    //Enable tabbing between text boxes
    if assigned(GUIC.activeTextBox) and KeyTyped(tabKey) then 
    begin
      r := GUIC.activeTextBox;
      pnl := r^.parent;
      txtCount := Length(pnl^.textBoxes);
      
      if KeyDown(LeftShiftKey) or KeyDown(RightShiftKey) then inc := -1
      else inc := +1;
      
      // Only tab if more than one textbox
      if txtCount > 1 then
      begin
        FinishReadingText();
        idx := (r^.elementIndex + inc) mod txtCount;
        if idx < 0 then idx := idx + txtCount;
        
        txt := @pnl^.textBoxes[idx];
        GUISetActiveTextbox(txt);
      end;
    end;
  end;
var
  pnl: Panel;
begin
  dialog.Complete := false;
  GUIC.doneReading := false;
  GUIC.lastClicked := nil;
  
  UpdateDrag();
  
  if AnyKeyPressed() then
    _UpdateTextEntry();
  
  pnl := PanelAtPoint(MousePosition());
  if MouseClicked(Leftbutton) then GUIC.panelClicked := pnl
  else GUIC.panelClicked := nil;
  
  if PanelActive(pnl) then
  begin
    HandlePanelInput(pnl);
  end;
  
  if assigned(GUIC.activeTextbox) and not ReadingText() then
    FinishReadingText();
    
  if PanelVisible(dialog.dialogPanel) then 
    UpdateFileDialog();
end;

procedure RegisterEventCallback(r: Region; callback: GUIEventCallback);
var
  rp: RegionPtr;
begin
  rp := ToRegionPtr(r);
  if not Assigned(rp) then exit;
  
  with rp^ do
  begin
    SetLength(callbacks, Length(callbacks) + 1);
    callbacks[High(callbacks)] := callback;
  end;
end;

//=============================================================================
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUserInterface', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    SetLength(GUIC.panels, 0);
    SetLength(GUIC.visiblePanels, 0);
    
    GUIC.globalGUIFont := nil;
    // Color set on loading window
    GUIC.VectorDrawing      := false;
    GUIC.lastClicked        := nil;
    GUIC.activeTextBox      := nil;
    GUIC.lastActiveTextBox  := nil;
    GUIC.doneReading        := false;
    GUIC.lastTextRead       := '';
    GUIC.downRegistered     := false;
    GUIC.panelDragging      := nil;
    
    GUIC.panelIds := TStringHash.Create(False, 1024);
    
    GUISetForegroundColor($ffffffff);
    GUISetBackgroundColor($00000000);
    
    with dialog do
    begin
      dialogPanel           := nil;
      currentPath           := '';     // The path to the current directory being shown
      currentSelectedPath   := '';     // The path to the file selected by the user
      cancelled             := false;
      allowNew              := false;
      selectType            := fdFilesAndDirectories;
      onlyFiles             := false;
      lastSelectedFile      := -1;
      lastSelectedPath      := -1;
      complete              := false;
    end;
        
    {$IFDEF TRACE}
      TraceExit('sgUserInterface', 'Initialise');
    {$ENDIF}
  end;

  finalization
  begin
    ReleaseAllPanels();
    FreeAndNil(GUIC.panelIds);
  end;

end.
