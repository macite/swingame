unit stringhash;

// Defines a hash table with string keys.
// this class is not derived from THashTable because method signatures are
// different, but it embeds one
// compatibility with *ix: check cases of unit names 

//=============================================================================
interface
//=============================================================================
  uses hashtable, comparable, sysutils;

  type
    tStrHashIterator = class(tHashTableIterator)
    public
      function getKey: string; reintroduce;
      property key: string read getKey;
      //protected
      //   constructor create(table: tHashTable);
    end;

    _StringObjectFactory = class(TObject)
    private
      fCaseSensitive: Boolean;
    public
      constructor create(isCaseSensitive: Boolean);
      function createObject(const value: string): tString;
    end;

    TStringHash = class(TObject)
    private
      fHashTable: tHashTable;
      fObjectFactory: _StringObjectFactory;
      protected
      procedure fSetValue(const key: string; value: TObject); virtual;
    public
      function getIterator: tStrHashIterator; virtual;
      function containsKey(const key: string): Boolean; virtual;
      function containsValue(value: TObject): Boolean; virtual;
      function getValue(const key: string): TObject; virtual;
      function setValue(const key: string; value: TObject): Boolean; virtual;
      function remove(const key: string): TObject; virtual;
      function getCount: integer; virtual;
      property values[key: string]: TObject read getValue write fsetValue;
      property count: integer read getCount;
      {$IFNDEF FPC}
      constructor create(caseSensitive: Boolean = false; initialcapacity: integer = 10);
      {$ELSE FPC}
      constructor create; overload;
      constructor create(caseSensitive: Boolean); overload;
      constructor create(caseSensitive: Boolean; initialcapacity: integer); overload;
      {$ENDIF FPC}

      destructor destroy; override;

      procedure clear; virtual;
      procedure deleteAll; virtual;
    end;

//=============================================================================
implementation
//=============================================================================

  // { $I sgTrace.inc}
  //   uses sgTrace;

  //---------------------------------------------------------------------------
  // tStrHashIterator - iterator for string hash table
  // basically an adapter shell for tMapIterator
  //---------------------------------------------------------------------------

  procedure throwTypeException(const className: string);
  begin
    raise exception.create('Wrong type. Expecting tString, got ' + className);
  end;

  function tStrHashIterator.getKey: string;
  var
    s: TObject;
  begin
    s := inherited getKey;
    if not (s is tString) then
      throwTypeException(s.ClassName);
    result := tString(s).value;
  end;

  (*
  constructor tStrHashIterator.create(iterator: tMapIterator);
  begin
    inherited create;
    fIterator := iterator;
  end;
  *)

//---------------------------------------------------------------------------
// _StringObjectFactory
//---------------------------------------------------------------------------

  constructor _StringObjectFactory.create(isCaseSensitive: Boolean);
  begin
    inherited create;
    fCaseSensitive := isCaseSensitive;
  end;

  function _StringObjectFactory.createObject(const value: string): tString;
  begin
    if fCaseSensitive then
      result := tString.create(value)
    else
      result := tStringNoCase.create(value);
  end;

//---------------------------------------------------------------------------
// TStringHash
//---------------------------------------------------------------------------

  procedure TStringHash.fSetValue(const key: string; value: TObject);
  begin
    setValue(key, value);
  end;

  function TStringHash.getIterator: tStrHashIterator;
  begin
    result := tStrHashIterator.create(fHashTable);
  end;

  function TStringHash.containsKey(const key: string): Boolean;
  var
    s: tString;
  begin
    s := fObjectFactory.createObject(key);
    try
      result := fHashTable.containsKey(s);
    finally
      s.free;
    end;
  end;

  function TStringHash.containsValue(value: TObject): Boolean;
  begin
    result := fHashTable.containsValue(tComparable(value))
  end;

  function TStringHash.getValue(const key: string): TObject;
  var
    s: tString;
  begin
    s := fObjectFactory.createObject(key);
    try
      result := fHashTable.getValue(s);
    finally
      s.free;
    end;
  end;

  function TStringHash.setValue(const key: string; value: TObject): Boolean;
  begin
    {$IFDEF TRACE}
      //TraceEnter('stringhash', 'TStringHash.setValue');
      //Trace('stringhash', 'Info', 'TStringHash.setValue', 'fObjectFactory = ' + HexStr(fObjectFactory));
      //Trace('stringhash', 'Info', 'TStringHash.setValue', 'fHashTable = ' + HexStr(fHashTable));
    {$ENDIF}
    result := fHashTable.setValue(fObjectFactory.createObject(key), value);
    {$IFDEF TRACE}
      //TraceExit('stringhash', 'TStringHash.setValue');
    {$ENDIF}
  end;

  function TStringHash.remove(const key: string): TObject;
  var
    s: tString;
  begin
    s := fObjectFactory.createObject(key);
    try
      result := fHashTable.remove(s);
    finally
      // s.free; //freed in fHashTable
    end;
  end;

  function TStringHash.getCount: integer;
  begin
    result := fHashTable.getCount;
  end;

  {$IFNDEF FPC}
  constructor TStringHash.create(caseSensitive: Boolean = false; initialcapacity: integer = 10);
  begin
    inherited create;
    fObjectFactory := _StringObjectFactory.create(caseSensitive);
    fHashTable := tHashTable.create(initialcapacity, 0.75);
  end;

  {$ELSE FPC}

  constructor TStringHash.create;
  begin
    inherited create;
    fObjectFactory := _StringObjectFactory.create(false);
    fHashTable := tHashTable.create;
  end;
  constructor TStringHash.create(caseSensitive: Boolean);
  begin
    inherited create;
    fObjectFactory := _StringObjectFactory.create(caseSensitive);
    fHashTable := tHashTable.create;
  end;

  constructor TStringHash.create(caseSensitive: Boolean; initialcapacity: integer);
  begin
    inherited create;
    fObjectFactory := _StringObjectFactory.create(caseSensitive);
    fHashTable := tHashTable.create(initialcapacity, 0.75, nil, true);
  end;
  {$ENDIF FPC}

  destructor TStringHash.destroy;
  begin
     fHashTable.free;
     fObjectFactory.free;
     inherited destroy;
  end;

  procedure TStringHash.clear;
  begin
    fHashTable.clear;
  end;

  procedure TStringHash.deleteAll;
  begin
    fHashTable.deleteAll;
  end;


end.

