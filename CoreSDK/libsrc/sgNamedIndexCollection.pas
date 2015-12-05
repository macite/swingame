//=============================================================================
// sgNamedIndexCollection.pas
//=============================================================================
//
// This private SwinGame library unit is responsible for managing named
// collections of indexes.
//
// This can be used to track names of resources in an array -- though the two
// need to be kept in sync (ensure add/remove update both).
//
//=============================================================================

unit sgNamedIndexCollection;
interface
  uses sgBackendTypes, sgTypes; //TODO: Remove sgTypes when removed StringArray
  
  /// Get the name of the value at index idx.
  ///
  function NameAt(const col: NamedIndexCollection; idx: Integer): String;
  
  /// Get the index of the specified name.
  ///
  function IndexOf(const col: NamedIndexCollection;const name: String): Integer;
  
  /// The number of names in the collection
  ///
  function NameCount(const col: NamedIndexCollection): Integer;

  function NamesOf(const col: NamedIndexCollection): StringArray;
  
  function HasName(const col: NamedIndexCollection;const name: String): Boolean;
  
  /// Add a new name to the index. Returns the index of the added element or
  /// -1 if the add fails.
  ///
  function AddName(var col: NamedIndexCollection;const name: String): Integer;

  //adds more than one name delimited by ',' i.e. name1,name2..
  Procedure AddNamesToCollection(var col: NamedIndexCollection;const names: String);

  /// returns names in an index collection in the following manner: name1,name2...
  function NamedIndexCollectionNameList(const list:NamedIndexCollection):String;
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection;const names: StringArray); overload;
  procedure InitNamedIndexCollection(var col: NamedIndexCollection); overload;
  procedure RemoveName(var col: NamedIndexCollection; idx: Longint); overload;
  function RemoveName(var col: NamedIndexCollection;const name: String): Longint; overload;
  procedure FreeNamedIndexCollection(var col: NamedIndexCollection);
  
  procedure RemoveAllNamesInCollection(var col: NamedIndexCollection);

implementation
uses sgShared, stringhash, sgSharedUtils, StrUtils;

  function NameAt(const col: NamedIndexCollection;idx: Integer): String;
  begin
    if (idx >= Low(col.names)) and (idx <= High(col.names)) then
      result := col.names[idx]
    else
      result := '';
  end;

    function NamedIndexCollectionNameList(const list:NamedIndexCollection):String;
  var
  i : Longint;
  begin
      result:=NameAt(list,0);
    for i:=1 to NameCount(list)-1 do
    begin
      result+=','+NameAt(list, i);
    end;
  end;


  function IndexOf(const col: NamedIndexCollection;const name: String): Integer;
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    if assigned(hash) and hash.containsKey(name) then
      result := TIntegerContainer(hash.values[name]).Value
    else
      result := -1;
  end;

  function NameCount(const col: NamedIndexCollection): Integer;
  begin
    result := Length(col.names);
  end;
  
  function NamesOf(const col: NamedIndexCollection): StringArray;
  begin
    result := col.names;
  end;
  
  function HasName(const col: NamedIndexCollection;const name: String): Boolean;
  var
    hash: TStringHash;
  begin
    result := false;
    hash := TStringHash(col.ids);
    
    if not assigned(hash) then exit;
    
    result := hash.containsKey(name);
  end;
  
  
  function AddName(var col: NamedIndexCollection;const name: String): Integer;
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    
    if assigned(hash) then
    begin
      if hash.containsKey(name) then begin RaiseException('Error: Adding ' + name + ' to the name collection twice.'); exit; end;
      
      SetLength(col.names, Length(col.names) + 1);            // Add to the names array
      result := High(col.names);                              // Record the index of the added name
      col.names[result] := name;                              // Store the name in the names array
      hash.setValue(name, TIntegerContainer.Create(result));  // Store this idx in the hashtable
    end
    else
      result := -1;                                           // Failed to add return -1 idx
  end;

  procedure AddNamesToCollection(var col: NamedIndexCollection;const names: String);
  var
    i, namesLength:Longint;    
  begin
    if Length(names) = 0 then exit;
    
    //number of names = ,'s + 1 (commas + 1)
    namesLength := CountDelimiter(names, ',') + 1;
    //WriteLn('Reading ', namesLength, ' names from ', names);
    
    for i := 1 to namesLength do
    begin
      AddName(col, ExtractDelimited(i,names,[',']));
    end;
  end;

  function RemoveName(var col: NamedIndexCollection;const name: String): Longint; overload;
  begin
    result := IndexOf(col, name);
    RemoveName(col, result);
  end;

  procedure RemoveName(var col: NamedIndexCollection; idx: Longint); overload;
  var
    hash: TStringHash;
    name: String;
    i: Integer;
  begin
    // WriteLn('remove name');
    hash := TStringHash(col.ids);
    if not Assigned(hash) then exit;

    name := NameAt(col, idx);
    // WriteLn(name);
    hash.remove(name).Free();

    for i := idx to High(col.names) - 1 do
    begin
      // WriteLn(i);
      col.names[i] := col.names[i + 1];
      TIntegerContainer(hash.values[col.names[i]]).Value := i;
    end;
    // WriteLn('length: ', Length(col.names));
    SetLength(col.names, Length(col.names) - 1);
  end;
  
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection;const names: StringArray); overload;
  var
    hash: TStringHash;
    i: Integer;
  begin
    // WriteLn('here');
    hash := TStringHash.Create(False, 1024);   //Create the hash locally and store in col.ids
    col.ids := hash;
    
    SetLength(col.names, Length(names));
    for i := Low(names) to High(names) do
    begin
      col.names[i] := names[i];
      hash.setValue(names[i], TIntegerContainer.Create(i));
    end;
  end;
  
  procedure InitNamedIndexCollection(var col: NamedIndexCollection); overload;
  var
    names: Array of String;
  begin
    SetLength(names, 0);
    
    InitNamedIndexCollection(col, names);
  end;
  
  procedure FreeNamedIndexCollection(var col: NamedIndexCollection);
  var
    hash: TStringHash;
  begin
    hash := TStringHash(col.ids);
    if assigned(hash) then
    begin
      hash.DeleteAll();
      hash.Free();
    end;
    col.ids := nil;
  end;


  procedure RemoveAllNamesInCollection(var col: NamedIndexCollection);
  var
    i : LongInt;
  begin
    for i := 0 to NameCount(col) - 1 do
      RemoveName(col, i);
  end;
end.
