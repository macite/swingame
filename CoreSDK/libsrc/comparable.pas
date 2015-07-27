unit comparable;

//=============================================================================
interface
//=============================================================================

  uses sysutils;

  type

    ECompareException = class(Exception);

    //-------------------------------------------------------------------------
    // Base object for collections to operate on all item objects should
    // inherit from it (TInteger, TString, etc)
    //-------------------------------------------------------------------------

    TComparable = class(tObject)
    protected
      function compareObjects(obj2: TComparable): integer; virtual; abstract;
    public
      function  hashCode: Int64; virtual; abstract;
      function compareTo(obj2: TComparable): integer;
    end;

//-------------------------------------------------------------------------
//  CLASSES FOR PRIMITIVE TYPES
//-------------------------------------------------------------------------

    // Derive all numeric classes from here
    TNumeric = class(TComparable)
    protected
      function compareObjects(obj2: TComparable): integer; override;
    public
      function asDouble: double; virtual; abstract;
    end;

    TInteger = class(TNumeric)
    protected
      fValue: integer;
    public
      property value: integer read fValue write fValue;
      function hashCode: Int64; override;
      constructor create(val: integer);
      function asDouble: double; override;
    end;

    TDouble = class(TNumeric)
    protected
      fValue: Double;
      fHashCode: Int64;
      procedure setValue(value: double);
    public
      property value: double read fValue write setValue;
      function hashCode: Int64; override;
      constructor create(val: double);
      function asDouble: double; override;
    end;

    TString = class(TComparable)
    protected
      fValue: string;
      fHashCode : Int64;

      procedure setValue(const value: string);
      function compareObjects(obj2: TComparable): integer; override;
    public
      property value: string read fValue write setvalue;
      function hashCode: Int64; override;
      constructor create(const val: string);
    end;

    TStringNoCase = class(TString)
    protected
      function compareObjects(obj2: TComparable): integer; override;
    end;

  //---------------------------------------------------------------------------

  function equal(obj1, obj2: TComparable): Boolean;
  function compare(Item1, Item2: TComparable): integer;

  procedure throwComparableException(obj: TComparable; targetClass: tClass);

//=============================================================================
implementation
//=============================================================================

  procedure throwComparableException(obj: TComparable; targetClass: tClass);
  begin
    if not (obj is targetClass) then
      raise ECompareException.create(targetClass.ClassName + ' cannot be compared with ' + obj.className);
  end;

//---------------------------------------------------------------------------
//  TComparable
//---------------------------------------------------------------------------

  function TComparable.compareTo(obj2: TComparable): integer;
  begin
    if obj2 = nil then
      result := -1
    else
      result := self.compareObjects(obj2);
  end;

//---------------------------------------------------------------------------
// TNumeric
//---------------------------------------------------------------------------

  function TNumeric.compareObjects(obj2: TComparable): integer;
  var
     otherDouble: double;
  begin
    throwComparableException(obj2, TNumeric);
    otherDouble := TNumeric(obj2).asDouble;
    if asDouble > otherDouble then
      result := 1
    else if asDouble < otherDouble then
      result := -1
    else result := 0;
  end;

//---------------------------------------------------------------------------
//  TInteger
//---------------------------------------------------------------------------

  function TInteger.hashCode: Int64;
  begin
    // make sure hash is never 0 so that it's different for nulls
    // - I don't even know why :)
    if fValue >= 0 then
      result := fValue + 1
    else
      result := fValue;
  end;

  constructor TInteger.create(val: integer);
  begin
    inherited create;
    value := val;
  end;

  function TInteger.asDouble: double;
  begin
    result := value;
  end;

//---------------------------------------------------------------------------
// TDouble
//---------------------------------------------------------------------------

  procedure TDouble.setValue(value: double);
  var
    max: Int64;
  begin
    fValue := value;
    
    max := High(Int64);
    
    // calculate hash code and store it
    while (value * 10) < max do value := value * 10;
    while value > max do value := value / 10;
    fHashCode := round(value);
  end;

  function TDouble.hashCode: Int64;
  begin
    result := fHashCode;
  end;

  constructor TDouble.create(val: double);
  begin
    inherited create;
    value := val;
  end;

  function TDouble.asDouble: double;
  begin
    result := value;
  end;

//---------------------------------------------------------------------------
// TStringNoCase
//---------------------------------------------------------------------------

  function TStringNoCase.compareObjects(obj2: TComparable): integer;
  begin
    throwComparableException(obj2, TString);
    result := CompareText(value, TString(obj2).value);
  end;

//---------------------------------------------------------------------------
//  TString 
//---------------------------------------------------------------------------

  function TString.compareObjects(obj2: TComparable): integer;
  begin
    throwComparableException(obj2, TString);
    result := CompareStr(value, TString(obj2).value);
  end;

  procedure TString.setValue(const value: string);
  var
     h, i: integer;
  begin
    fValue := value;
    // calculate hash code and store it
    h := 0;
    for i := 1 to length(value) do
      h := h * 31 + integer(value[i]);
    fHashCode := h;
  end;

  function TString.hashCode: Int64;
  begin
    result := fHashCode;
  end;

  constructor TString.create(const val: string);
  begin
    inherited create;
    value := val;
  end;

//---------------------------------------------------------------------------
//  FUNCTIONS
//---------------------------------------------------------------------------

  function equal(obj1, obj2: TComparable): Boolean;
  begin
    if ((obj1 = nil) and (obj2 = nil)) or (obj1 = obj2) then
      result := true
    else if obj1 = nil then
      result := false
    else
      result := (obj1.classType = obj2.classType) and (obj1.compareto(obj2) = 0);
  end;

  function compare(Item1, Item2: TComparable): integer;
  begin
    if ((Item1 = nil) and (Item2 = nil)) or (Item1 = Item2) then
      result := 0
    else if item1 = nil then
      result := -1
    else
      result := item1.compareTo(item2);
  end;


end.
