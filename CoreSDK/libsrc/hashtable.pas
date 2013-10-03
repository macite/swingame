unit hashtable;

// When compiling with freePascal, use -S2 switch (-Sd won't work)
// TODO: see if making it compatible with -Sd is possible and / or necessary
//
//  (incomplete) rip-off of java hashtable


//=============================================================================
interface
//=============================================================================

  uses comparable, sysutils;

  type

    THashEntry = class(TComparable)
    private
      fkey: TComparable;
      fvalue: TObject;
    protected
      function compareObjects(object2: TComparable): integer; override;
    public
      next: THashEntry;
      function  hashCode: Int64; override;
      function getKey: TComparable;
      function getValue: TObject;
      procedure setValue(avalue: TObject);
      property key: TComparable read getKey;
      property value: TObject read getValue write setValue;
      constructor create(akey: TComparable; avalue: TObject);
    end;

    THashEntryClass = class of THashEntry;

    THashEntryFactory = class(TObject)
    private
      fHashClass: THashEntryClass;
    public
      function getEntry(key: TComparable; value: TObject): THashEntry; virtual;
      constructor create(hashEntryClass: THashEntryClass);
    end;

    TMapIterator = class (TObject)
    public
      procedure next; virtual; abstract;
      procedure remove; virtual; abstract;
      function hasNext: Boolean; virtual; abstract;
      function getKey: TComparable; virtual; abstract;
      function getValue: TObject; virtual; abstract;
      function validEntry: Boolean; virtual; abstract;
      procedure setValue(value: TObject); virtual; abstract;
      function isValid: Boolean; virtual; abstract;
      property key: TComparable read getKey;
      property value: TObject read getValue write setValue;
    end;

    {$IFNDEF FPC}
    THashEntryTable = array of THashEntry;
    HashEntryTablePtr = ^THashEntryTable;
    {$ELSE FPC}
    HashEntryTablePtr = ^THashEntry;
    {$ENDIF}

    THashTable = class (TObject)
    protected
      fTable: HashEntryTablePtr;
      fEntryFactory: THashEntryFactory;
      fModCount: integer;
      fCapacity: integer;
      fThreshHold: integer;
      fLoadFactor: real;
      fCount     : integer;
      fOwnKeys   : Boolean;
      function hashToIndex(key: TComparable): integer;
      procedure rehash; virtual;

      // there's a potential for memory leak here
      // - if key already in the table, it does not get re-inserted.
      // function setvalue returns false if this happens, so key may be freed
      // if necessary but procedure fsetvalue does not free keys.
      // SO: don't use values property unless you're managing _all_ keys
      // somewhere outsite of the hash table
      // CORRECTION: if ownKeys is set, fSetvalue will free the key, so there's
      // no leak
      // Another possibility for a leak - what happens if the value that is
      // already in the table is not managed outside? Should be deleted, but how?
      procedure fSetValue(key: TComparable; value: TObject); virtual;
      procedure clearTable(deleteValues: Boolean);
    public
      function getIterator: TMapIterator; virtual;
      function containsKey(key: TComparable): Boolean; virtual;
      function containsValue(value: TObject): Boolean; virtual;
      function getValue(key: TComparable): TObject; virtual;
      function setValue(key: TComparable; value: TObject): Boolean; virtual;
      function remove(key: TComparable): TObject; virtual;
      function getCount: integer; virtual;
      property values[key: TComparable]: TObject read getValue write fsetValue;
      property count: integer read getCount;
      {$IFNDEF FPC}
      constructor create(initialcapacity: integer = 10; loadfactor: real = 0.75;
                         entryfactory: THashEntryFactory = nil; ownKeys: Boolean = true);
      {$ELSE FPC}
      constructor create(initialcapacity: integer; loadfactor: real;
                         entryfactory: THashEntryFactory; ownKeys: Boolean); overload;
      constructor create; overload;
      {$ENDIF}
      destructor destroy; override;
      procedure clear; virtual;
      procedure deleteAll; virtual;
    end;

    THashTableIterator = class(TMapIterator)
    protected
      fIndex: integer;
      fEntry: THashEntry;
      fModCount: integer;
      fIsRemoved: Boolean;
      fCurrent: integer;
      fHashTable: THashTable;
    public
      constructor create(table: THashTable);
      procedure next; override;
      procedure remove; override;
      function hasNext: Boolean; override;
      function getKey: TComparable; override;
      function getValue: TObject; override;
      procedure setValue(avalue: TObject); override;
      function validEntry: Boolean; override;
      function isValid: Boolean; override;
    end;



//=============================================================================
implementation
//=============================================================================

//---------------------------------------------------------------------------
// array get/put methods - pointer functions
//---------------------------------------------------------------------------

  {$IFNDEF FPC}
  //delphi implementation, uses dynamic arrays

  function getNewEntryTable(size: integer): HashEntryTablePtr;
  begin
    new(result);
    setLength(result^, size);
  end;

  procedure freeEntryTable(table: HashEntryTablePtr; oldSize: integer);
  begin
    setLength(table^, 0);
    dispose(table);
  end;

  function arrayGet(arr: HashEntryTablePtr; index: integer): THashEntry;
  begin
    result := arr^[index];
  end;

  procedure arrayPut(arr: HashEntryTablePtr; index: integer; value: THashEntry);
  begin
    arr^[index] := value;
  end;

  {$ELSE FPC}

  // freepascal implementaion, uses pointers as arrays

  function getNewEntryTable(size: integer): HashEntryTablePtr;
  var
    i: Integer;
  begin
    getmem(result, size * sizeOf(THashEntry));
    //set all positions to nil
    for i := 0 to size - 1 do
    begin
      (result + i)^ := nil;
    end;
  end;

  procedure freeEntryTable(table: HashEntryTablePtr; oldSize: integer);
  begin
    freemem(table, oldSize * sizeOf(THashEntry));
  end;

  function arrayGet(arr: HashEntryTablePtr; index: integer): THashEntry;
  begin
    result := (arr + index)^; //arr[index];
  end;

  procedure arrayPut(arr: HashEntryTablePtr; index: integer; value: THashEntry);
  begin
    (arr + index)^ := value; //arr[index] := value;
  end;

  {$ENDIF FPC}


  function equal(item1, item2 :TObject): Boolean;
  begin
    if ((item1 = nil) or (item1 is TComparable)) and ((item2 = nil) or (item2 is TComparable)) then
      result := comparable.equal(TComparable(item1), TComparable(item2))
    else
      result := false;
  end;


  constructor THashEntryFactory.create(hashEntryClass: THashEntryClass);
  begin
    inherited create;
    fHashClass := hashEntryClass;
  end;

  function THashEntryFactory.getEntry(key: TComparable; value: TObject): THashEntry;
  begin
    result := fHashClass.create(key, value);
  end;

//---------------------------------------------------------------------------
// THashEntry
//---------------------------------------------------------------------------

  function THashEntry.compareObjects(object2: TComparable): integer;
  begin
    throwComparableException(object2, self.ClassType);
    // this is not really important so we'll just make it compare keys
    result := compare(fkey, THashEntry(object2).key);
  end;

  function  THashEntry.hashCode: Int64;
  begin
    // for our purposes the hash code of the key is good enough
    result := key.hashCode;
  end;

  function THashEntry.getKey: TComparable;
  begin
    result := fKey;
  end;

  function THashEntry.getValue: TObject;
  begin
    result := fValue;
  end;

  procedure THashEntry.setValue(avalue: TObject);
  begin
    fValue := avalue;
  end;

  constructor THashEntry.create(akey: TComparable; avalue: TObject);
  begin
    inherited create;
    fKey := akey;
    fValue := avalue;
    next := nil;
  end;


//---------------------------------------------------------------------------
// hashtable
//---------------------------------------------------------------------------

  function THashTable.hashToIndex(key: TComparable): integer;
  begin
    result := Integer(abs(key.hashCode) mod fCapacity);
  end;

  procedure THashTable.rehash;
  var
    oldCapacity: integer;
    oldTable: HashEntryTablePtr;
    newCapacity: integer;
    newTable: HashEntryTablePtr;
    i: integer;
    index: integer;
    entry, oldentry: THashEntry;
  begin

    oldCapacity := fCapacity;
    newCapacity := oldCapacity * 2 + 1;
    newTable    := getNewEntryTable(newCapacity);

    inc(fModCount);
    oldTable := fTable;
    fTable := newTable;
    fCapacity := newCapacity;
    fThreshHold := Integer(round(newCapacity * fLoadFactor));

    try
      for i := 0 to oldCapacity - 1 do begin
        oldEntry := arrayGet(oldTable, i);
        while oldEntry <> nil do begin
          entry := oldEntry;
          oldEntry := oldEntry.next;
          index := hashToIndex(entry.key);
          entry.next := arrayGet(fTable, index);
          arrayPut(fTable, index, entry);
        end;
      end;
    finally
      freeEntryTable(oldTable, oldCapacity);
    end;
  end;

  procedure THashTable.fSetValue(key: TComparable; value: TObject);
  begin
    setValue(key, value);
  end;

  function THashTable.getIterator: TMapIterator;
  begin
    result := THashTableIterator.create(self);
  end;

  function THashTable.containsKey(key: TComparable): Boolean;
  var
    idx: integer;
    entry: THashEntry;
  begin
    idx := hashToIndex(key);
    result := false;
    entry := arrayGet(fTable, idx);
    while (entry <> nil) and not result do begin
      result := equal(key, entry.key);
      entry := entry.next;
    end;
  end;

  function THashTable.containsValue(value: TObject): Boolean;
  var
    idx: integer;
    entry: THashEntry;
  begin
    result := false;
    for idx := 0 to fCapacity - 1 do begin
      entry := arrayGet(fTable, idx);
      while (entry <> nil) and not result do begin
        result := equal(value, entry.value);
        entry := entry.next;
      end;
      if result then break;
    end;
  end;

  function THashTable.getValue(key: TComparable): TObject;
  var
    idx: integer;
    entry: THashEntry;
  begin
    idx := hashToIndex(key);
    result := nil;
    entry := arrayGet(fTable, idx);
    while (entry <> nil) do begin
      if equal(key, entry.key) then begin
        result := entry.value;
        break;
      end;
      entry := entry.next;
    end;
  end;

  function THashTable.setValue(key: TComparable; value: TObject): Boolean;
  var
    idx: integer;
    entry: THashEntry;
  begin
    // {$IFDEF TRACE}
    //   TraceEnter('hashtable', 'THashTable.setValue');
    //   Trace('hashtable', 'Info', 'THashTable.setValue', 'fTable = ' + HexStr(fTable));
    // {$ENDIF}

    // first try to find key in the table and replace the value
    idx := hashToIndex(key);
    // {$IFDEF TRACE}
    //     Trace('hashtable', 'Info', 'THashTable.setValue', 'idx = ' + IntToStr(idx));
    // {$ENDIF}
    entry := arrayGet(fTable, idx);
    // {$IFDEF TRACE}
    //     Trace('hashtable', 'Info', 'THashTable.setValue', 'entry = ' + HexStr(entry));
    // {$ENDIF}
    while entry <> nil do
    begin
      if equal(key, entry.key) then
      begin
        // {$IFDEF TRACE}
        //   Trace('hashtable', 'Info', 'THashTable.setValue', 'Found Matching');
        // {$ENDIF}

        result := false;
        entry.value := value;
        if fOwnKeys then key.free;
        exit;
      end;
      entry := entry.next;
    end;
    
    // {$IFDEF TRACE}
    //     Trace('hashtable', 'Info', 'THashTable.setValue', 'Done checking... inserting');
    // {$ENDIF}

    // inserting new key-value pair
    inc(fModCount);
    if fcount > fThreshHold then
      rehash;

    idx := hashToIndex(key);
    entry := fEntryFactory.getEntry(key, value);
    entry.next := arrayGet(ftable ,idx);
    arrayPut(ftable, idx, entry);
    inc(fcount);
    result := true;
    
    // {$IFDEF TRACE}
    //   TraceExit('hashtable', 'THashTable.setValue');
    // {$ENDIF}
  end;


  function THashTable.remove(key: TComparable): TObject;
  var
    idx: integer;
    entry: THashEntry;
    preventry: THashEntry;
  begin
    idx := hashToIndex(key);
    entry := arrayGet(fTable, idx);
    result := nil;
    prevEntry := nil;
    while entry <> nil do begin
      if equal(key, entry.key) then begin
        inc(fModCount);
        result := entry.value;
        if fOwnKeys then if entry.key <> key then key.free; //test this!
        if fOwnKeys then entry.key.free;
        if prevEntry = nil then
           arrayPut(fTable, idx, entry.next)
        else
           prevEntry.next := entry.next;
        entry.free;
        dec(fCount);
        break;
      end;
      preventry := entry;
      entry := entry.next;
    end;

  end;

  function THashTable.getCount: integer;
  begin
    result := fCount;
  end;

  {$IFDEF FPC}
  constructor THashTable.create(initialcapacity: integer; loadfactor: real;
                                entryfactory: THashEntryFactory; ownKeys: Boolean);
  begin
    inherited create;
    fLoadFactor := loadfactor;
    fOwnKeys := ownKeys;
    if entryFactory = nil then
      fEntryFactory := THashEntryFactory.create(THashEntry)
    else
      fEntryFactory := entryfactory;
    fTable := getNewEntryTable(initialCapacity);
    fCapacity := initialcapacity;
    fThreshHold := Integer(round(fCapacity * fLoadFactor));
    fCount := 0;
    fModCount := 0;
  end;

  constructor THashTable.create;
  begin
    create(10, 0.75, nil, true);
  end;

  {$ELSE FPC}
  constructor THashTable.create(initialcapacity: integer = 10; loadfactor: real = 0.75;
                                entryfactory: THashEntryFactory = nil; ownKeys: Boolean = true);
  begin
    inherited create;
    fLoadFactor := loadfactor;
    fOwnKeys := ownKeys;
    if entryFactory = nil then
      fEntryFactory := THashEntryFactory.create(THashEntry)
    else
      fEntryFactory := entryfactory;
    fTable := getNewEntryTable(initialCapacity);
    fCapacity := initialcapacity;
    fThreshHold := round(fCapacity * fLoadFactor);
    fCount := 0;
    fModCount := 0;
  end;
  {$ENDIF}

  destructor THashTable.destroy;
  begin
    clear;
    freeEntryTable(fTable, fCapacity);
    fEntryFactory.free;
    inherited;
  end;

  procedure THashTable.clear;
  begin
    clearTable(false);
  end;

  procedure THashTable.deleteAll;
  begin
    clearTable(true);
  end;

  procedure THashTable.clearTable(deleteValues: Boolean);
  var
    idx: integer;
    entry: THashEntry;
    temp : THashEntry;
  begin
    for idx := 0 to fCapacity - 1 do begin
      entry := arrayGet(ftable, idx);
      while entry <> nil do begin
        temp := entry;
        entry := entry.next;
        if fOwnKeys then
          temp.key.free;
        if deleteValues then
          temp.value.free;
        temp.free;
      end;
      arrayPut(fTable, idx, nil);
    end;
  end;

  //* iterator *
  constructor THashTableIterator.create(table: THashTable);
  var
    i: integer;
  begin
    inherited create;

    fHashTable := table;
    fModCount := table.fModCount;

    fIsRemoved := false;
    fCurrent := 0;

    fEntry := nil;
    // get first element
    if fHashTable.count > 0 then
      for i := 0 to fHashTable.fCapacity - 1 do begin
        fEntry := arrayGet(fHashTable.ftable, i);
        if fEntry <> nil then begin
          fIndex := i;
          break;
        end;
      end;
  end;


  procedure THashTableIterator.next;
  var
    i: integer;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');

    if fIsRemoved then
      fisRemoved := false
    else if fCurrent < fHashTable.count then begin
      fEntry := fEntry.next;
      if fEntry = nil then
        for i := fIndex + 1 to fHashTable.fCapacity - 1 do
          if arrayGet(fHashTable.fTable, i) <> nil then begin
            fEntry := arrayGet(fHashTable.fTable, i);
            fIndex := i;
            break;
          end;

      if fEntry <> nil then
        inc(fCurrent);
    end;
  end;

  function THashTableIterator.isValid: Boolean;
  begin
    result := fModCount = fHashTable.fModCount;
  end;

  procedure THashTableIterator.remove;
  var
    oldEntry: THashEntry;
    i: integer;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');

    if fIsRemoved or (fEntry = nil) then exit;

    oldEntry := fEntry;

    if fCurrent < fHashTable.count then begin
      fEntry := fEntry.next;
      if fEntry = nil then begin
        for i := fIndex + 1 to fHashTable.fCapacity - 1 do
          if arrayGet(fHashTable.fTable, i) <> nil then begin
            fEntry := arrayGet(fHashTable.fTable, i);
            fIndex := i;
            break;
          end;
      end;
    end;

    fHashTable.remove(oldEntry.key);
    fIsRemoved := true;
    fModCount := fHashTable.fmodCount;
  end;


  function THashTableIterator.hasNext: Boolean;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');
    result := (fCurrent < (fHashTable.count - 1)) or (fIsRemoved and (fCurrent < fHashTable.count));
  end;

  function THashTableIterator.getKey: TComparable;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');
    if not (fIsRemoved or (fEntry = nil)) then
      result := fEntry.key
    else
      result := nil;
  end;

  procedure THashTableIterator.setValue(avalue: TObject);
  begin
    // NOTE! at this point, dealing with the value that is being replaced is
    // the responsibility of the user
    if not isValid then
      raise exception.create('Iterator no longer valid');
    if validEntry then
      fEntry.value := avalue
    else
      raise exception.create('The entry is not valid');
  end;

  function THashTableIterator.getValue: TObject;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');
    if not (fIsRemoved or (fEntry = nil)) then
      result := fEntry.value
    else
      result := nil;
  end;

  function THashTableIterator.validEntry: Boolean;
  begin
    if fModCount <> fHashtable.fModCount then
      raise exception.create('Iterator no longer valid');
    result := (fEntry <> nil) and (fCurrent < fHashTable.count) and not fIsRemoved;
  end;

end.
