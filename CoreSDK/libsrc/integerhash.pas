unit integerhash;

//=============================================================================
interface
//=============================================================================

  uses hashtable, comparable, sysutils;

  type

    TIntHashIterator = class(THashTableIterator)
    public
      function getKey: integer; reintroduce;
      property key: integer read getKey;
      //   property value: TObject read getValue;
      //protected
      //   constructor create(table: THashTable);
    end;


    TIntegerHash = class(TObject)
    private
      fHashTable: THashTable;
    protected
      procedure fSetValue(key: integer; value: TObject); virtual;
    public
      function getIterator: TIntHashIterator; virtual;
      function containsKey(key: integer): Boolean; virtual;
      function containsValue(value: TObject): Boolean; virtual;
      function getValue(key: integer): TObject; virtual;
      function setValue(key: integer; value: TObject): Boolean; virtual;
      function remove(key: integer): TObject; virtual;

      function getCount: integer; virtual;

      property values[key: integer]: TObject read getValue write fsetValue;
      property count: integer read getCount;

      {$IFNDEF FPC}
      constructor create(initialcapacity: integer = 10);
      {$ELSE FPC}
      constructor create;
      constructor create(initialcapacity: integer);
      {$ENDIF FPC}

      destructor destroy; override;

      procedure clear; virtual;
      procedure deleteAll; virtual;
    end;

//=============================================================================
implementation
//=============================================================================

  // tSHIterator - iterator for integer hash table
  // basically an adapter shell for tMapIterator

  procedure throwTypeException(const className: string);
  begin
    raise exception.create('Wrong type. Expecting tInteger, got ' + className);
  end;

  function TIntHashIterator.getKey: integer;
  var
    s: TObject;
  begin
    s := inherited getKey;
    if not (s is tInteger) then
      throwTypeException(s.ClassName);
    result := tInteger(s).value;
  end;


  (*
  constructor TIntHashIterator.create(table: THashTable);
  begin
       inherited create(table);
  end;
  *)

//---------------------------------------------------------------------------
// TIntegerHash
//---------------------------------------------------------------------------

  procedure TIntegerHash.fSetValue(key: integer; value: TObject);
  begin
    setValue(key, value);
  end;

  function TIntegerHash.getIterator: TIntHashIterator;
  begin
    result := TIntHashIterator.create(fHashTable);
  end;

  function TIntegerHash.containsKey(key: integer): Boolean;
  var
    s: tInteger;
  begin
    s := tInteger.create(key);
    try
      result := fHashTable.containsKey(s);
    finally
      s.free;
    end;
  end;

  function TIntegerHash.containsValue(value: TObject): Boolean;
  begin
    result := fHashTable.containsValue(tComparable(value))
  end;

  function TIntegerHash.getValue(key: integer): TObject;
  var
    s: tInteger;
  begin
    s := tInteger.create(key);
    try
      result := fHashTable.getValue(s);
    finally
      s.free;
    end;
  end;

  function TIntegerHash.setValue(key: integer; value: TObject): Boolean;
  begin
    result := fHashTable.setValue(tInteger.create(key), value);
  end;

  function TIntegerHash.remove(key: integer): TObject;
  var
    s: tInteger;
  begin
    s := tInteger.create(key);
    try
      result := fHashTable.remove(s);
    finally
      //s.free;
    end;
  end;

  function TIntegerHash.getCount: integer;
  begin
    result := fHashTable.getCount;
  end;

  {$IFNDEF FPC}
  constructor TIntegerHash.create(initialcapacity: integer = 10);
  begin
    inherited create;
    fHashTable := THashTable.create(initialcapacity);
  end;

  {$ELSE FPC}

  constructor TIntegerHash.create;
  begin
    inherited create;
    fHashTable := THashTable.create;
  end;

  constructor TIntegerHash.create(initialcapacity: integer);
  begin
    inherited create;
    fHashTable := THashTable.create(initialcapacity, 0.75, nil, true);
  end;
  {$ENDIF FPC}

  destructor TIntegerHash.destroy;
  begin
    fHashTable.destroy;
    inherited destroy;
  end;

  procedure TIntegerHash.clear;
  begin
    fHashTable.clear;
  end;
  procedure TIntegerHash.deleteAll;
  begin
    fHashTable.deleteAll;
  end;

end.
