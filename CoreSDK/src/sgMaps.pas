

unit sgMaps;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
interface
  uses
    sgTypes, sgVectorShapes;
  
  type
    Direction = (mdNorthWest, mdNorth, mdNorthEast, mdWest, mdEast, mdSouthWest, mdSouth, mdSouthEast);
    
    {BitmapCell = Record
      Cell:         Longint;
      Bmap:         Bitmap;
    end;}
    
    BitmapCellKind = record
      Cell:         Longint;
      Bmap:         Bitmap;
      KindIdx:      Longint;    
    end;

    {Marker = record
      position:     Point2D;
      Id:           Longint;
    end;}

    BitmapCellKindArray = Array of BitmapCellKind;

    BitmapCellKindPtr = ^BitmapCellKind;
    Tile = ^TileData;
    TileData = Record
      TileID:             Longint;                      // The tile's unique id
      Kind:               Longint;                      // The "kind" of the tile - links to KindNames etc.
      Position:           Point2D;                      // Position of the top right corner of the Tile
      Center:             Point2D;                      // position of the center of the Tile
      TileBitmapCellKind: Array of BitmapCellKindPtr;      // ptr to bitmapCellKindData
      TileShape:          Shape;                        // shape of tile
      Values:             Array of Single;              // Array of single values.
      SurroundingTiles:   Array[Direction] of Tile;     // The adjcent tiles, can be nil if none.
    end;

    
    Map = ^MapData;
    MapData = Record
      name:              String;
      filename:           String;
      Tiles:              Array of Array of TileData;     // The actual tiles -> in col,row order
      SelectedTiles:      LongintArray;                   // id of selected tiles
      Isometric:          Boolean;                        // Map Isometric
      valueIds:           NamedIndexCollection;           // has names of values
      kindids:            NamedIndexCollection;           // has names of kinds
      //MarkerIds:          NamedIndexCollection;           // has names of Markers
      //MapMarkers:        Array of Marker;
      MapPrototype:       ShapePrototype;                 // prototype of the tiles
      MapHighlightcolor:  Color;                          // highlight color
      MapWidth:           Longint;                        // The Map width (# of grid in x axis)
      MapHeight:          Longint;                        // The Map height (# of grid in Y axis)
      MapLayer:           Longint;                        // The Number of layers within the map
      TileWidth:          Longint;                        // The Tile width
      TileHeight:         Longint;                        // The Tile height
      TileStagger:        Vector;                         // Offset of the tile's X Position
      MapDefaultValues:   Array of Array of Single;       //default values of tiles depending on bitmaps.
      BitmapCellKind:     Array of BitmapCellKind;        // Bitmap/cell/kinds.
    end;

//----------------------------------------------------------------------------
//       Load function            
//----------------------------------------------------------------------------

  /// Loads the Map from a text file which contains the map informations.
  /// this returns a map.
  ///
  /// @lib LoadMap
  ///
  /// @class Map
  /// @constructor
  /// @csn initForPath:%s

  function LoadMap(filename:string): Map;
  
  
  
//----------------------------------------------------------------------------
//       Draw Procedures            
//----------------------------------------------------------------------------


  /// Draws the map at the current camera position with the given map information.
  /// The width and height of the area drawn is the same as the screen size. 
  ///
  /// @lib DrawMap
  /// @sn drawMap:%s 
  ///
  /// @class Map
  /// @method Draw
  procedure DrawMap(map: Map); overload;

  /// Draws the map with the given camera position with the given map information.
  /// The width and height of the area drawn the screen size subtracted by the offset. 
  ///
  /// @lib DrawMapWithOffset
  /// @sn drawMap:%s WithOffset:%s
  ///
  /// @class Map
  /// @overload Draw DrawWithOffset
  procedure DrawMap(map: Map; offset:Vector); overload;

  /// Draws the map with debug information like surrounding tiles, kind and value.
  /// The visible area is the same size as the screen size.
  ///
  /// @lib DrawMapDebug
  ///
  /// @class Map
  /// @method DrawDebug
  procedure DrawMapDebug(map: map);

  /// Draws the outline of each tile at the current camera position
  /// given the map information.
  ///
  /// @lib DrawMapGrid
  ///
  /// @class Map
  /// @method DrawGrid
  procedure DrawMapGrid(m:map); overload;

  /// Draws the outline of each tile at the current camera position
  /// given the map information.
  ///
  /// @lib DrawMapGrid
  /// @sn drawMapGrid:%s WithOffset:%s
  ///
  /// @class Map
  /// @method DrawGridWithOffset
  procedure DrawMapGrid(m:map; offset:Vector); overload;

  
  /// Draws a single layer of the map's bitmap
  ///
  /// @lib DrawMapLayer
  /// @sn map:%s offset:%s layer :%s
  ///
  /// @class Map
  /// @method DrawLayers
  procedure DrawMapLayer(map: Map; offset:Vector; layer:Longint); overload;
  
  
  
//----------------------------------------------------------------------------
//       Save map
//----------------------------------------------------------------------------

  /// Saves map into a text file the filename provided needs to be the fullpath.
  ///
  /// @lib SaveMap
  ///
  /// @class Map
  /// @method SaveMap  
  procedure SaveMap(m:Map; filename:String);
  
  
  
//----------------------------------------------------------------------------
//       Selecting Tile          
//----------------------------------------------------------------------------
  
  /// Returns true if tile is in the selectedTile array in the map.
  ///
  /// @lib TileSelected
  /// @sn tile ****************************
  ///
  /// @class Tile
  /// @method Selected
  function TileSelected(map:Map; tile:Tile):Boolean;

  /// updates whether a tile should be selected or deselected.
  /// Tile to be selected or deselected depends on what is under the cursor.
  ///
  /// @lib UpdateSelect
  ///
  /// @class Map
  /// @method Update 
  procedure UpdateSelect(map:Map);

  /// If the tile passed in is selected this will deselect the tile
  /// and remove it from the selectedTiles array.
  ///
  /// @lib DeselectTile
  /// 
  /// @class Tile
  /// @method Deselect 
  procedure Deselect(map:Map; tile:Tile);

  /// Highlights a tile by drawing its shape using
  /// the highlight colour in the map. This highglights tiles with no offset.
  ///
  /// @lib HighlightTile
  ///
  /// @class Tile
  /// @method Highlight
  procedure HighlightTile(highlightedTile: Tile); OverLoad;

  /// Highlights a tile by drawing its shape using
  /// the highlight colour in the map. This highglights
  /// tiles with the given offset.
  ///
  /// @lib HighlightTileWithOffset
  /// @sn highlight:%s WithOffset:%s
  ///
  /// @class Tile
  /// @method HighlightWith Offset
  
  procedure HighlightTile(highlightedTile: Tile; offset: Vector); OverLoad;
  
  
  
//----------------------------------------------------------------------------
//collision functions and procedures
//----------------------------------------------------------------------------
  
  /// Returns a bounding rectangle the encompasses all the potential tiles
  ///that the sprite may collide with
  ///
  /// @lib GetPotentialCollisions
  /// @sn map:%s getPotentialCollisionsForKind:%s sprite:%s
  /// @class Map
  /// @method GetPotentialCollisions
  function  GetPotentialCollisions(map: Map; s: Sprite): Rectangle;

  /// Returns a Boolean of whether a sprite has collided with a tile of specified kind
  /// it also returns the cell column and row of the tile that the sprite collided with.
  ///
  /// @lib SpriteHasCollidedWithTile
  /// @sn map:%s hasCollidedWithKind:%s forSprite:%s resultAtX:%s y:%s 
  ///
  /// @class Map
  /// @method SpriteHasCollidedWithTile
  /// @csn kind:%s hasCollidedWithSprite:%s resultAtX:%s y:%s
  function SpriteHasCollidedWithTile(map: Map; k: Longint; s: Sprite; out collidedX, collidedY: Longint): Boolean; overload;

  /// Moves the sprite out of a tile. The x and y is the column and row of
  /// the tile that the sprite needs to be kicked out of.
  ///
  /// @lib MoveOut
  /// @sn
  ///
  /// @class Map
  /// @method MoveOut
  procedure MoveOut(m:map; s: Sprite; x, y: Longint);
  
  
  
//----------------------------------------------------------------------------
// Return map properties functions
//----------------------------------------------------------------------------
  
  /// Returns the BitmapCellKind of the map
  ///
  /// @lib BitmapCellKind
  ///
  /// @class
  function BitmapCellKinds(m:map): BitmapCellKindArray;

  ///returns the number of tiles that are in a map.
  ///
  /// @lib TileCount
  ///
  /// @class Map
  function TileCount(m : map): Longint;
  
  /// returns the index of the kind from the NamedIndexCollection given
  /// the name of the kind
  ///
  /// @lib IndexOfkind
  /// @sn map:%s indexOfKind:%s
  ///
  /// @class Map
  /// @getter IndexOfKind
  function IndexOfKind(const m :  map; const kname : string) : Longint;

  /// returns the index of the value from the NamedIndexCollection given
  /// the name of the value
  ///
  /// @lib IndexOfValue
  /// @sn map:%s indexOfValue:%s
  ///
  /// @class Map
  /// @getter IndexOfValue
  function IndexOfValues(const m :  map; const vName : string) : Longint;

  /// returns all the names of kinds in a string array
  ///
  /// @lib MapKinds
  ///
  /// @class Map
  /// @getter MapKinds
  function MapKinds(m: map) : StringArray;

  /// returns all the names of values in a string array
  ///
  /// @lib MapValues
  ///
  /// @class Map
  /// @getter MapValues
  function MapValues(m: map) : StringArray;

  /// Returns the stagger values of the given map.
  ///
  /// @lib TileStagger
  ///
  /// @class Map
  /// @getter TileStagger
  function TileStagger(m: Map) : Vector;

  /// Returns the value from the map default value given the kind id and value id
  ///
  /// @lib MapDefaultValues
  /// @sn map:%s defaultValueAtKindId:%s and valueId:%s
  ///
  /// class Map
  /// @csn defaultValueAtKindId:%s and valueId:%s
  function MapDefaultValues(m : map; const kId, vId : Longint): single;

  /// Returns the number of layers that the given map has.
  ///
  /// @lib LayerCount
  ///
  /// @class Map
  /// @getter LayerCount
  function LayerCount(m: Map) : Longint;
  /// Returns the Tile height of the given map.
  ///
  /// @lib TileHeight
  ///
  /// @class Map
  /// @getter TileHeight  
  function TileHeight(m: Map) : Longint;
  /// Returns the Tile width of the given map.
  ///
  /// @lib TileWidth
  ///
  /// @class Map
  /// @getter TileWidht
  function TileWidth(m: Map) : Longint;
  /// Returns the Color that is used for highlighting within the map.
  ///
  /// @lib MapHighlightColor
  ///
  /// @class Map
  /// @getter HighlightColor
  function MapHighlightcolor(m: Map) : color;
  /// Returns the number of selected tiles.
  ///
  /// @lib CountSelectedTiles
  function CountSelectedTiles(m: Map) : Longint;

  /// Returns the prototype of the tiles used within the map.
  ///
  /// @lib MapPrototype
  ///
  /// @class Map
  /// @getter Prototype
  function MapPrototype(m: Map) : ShapePrototype;

  /// Returns the number of rows within the given map
  ///
  /// @lib MapHeight
  ///
  /// @class Map
  /// @getter height  
  function MapHeight(m: Map) : Longint;

  /// Returns the number of columns within the given map
  ///
  /// @lib MapWidht
  ///
  /// @class Map
  /// @getter width
  function MapWidth(m: Map) : Longint;

  /// Returns the name of the value given the index.
  ///
  /// @lib ValueName
  /// @sn map:%s valueNameAtId:%s
  ///
  /// @class Map
  /// @getter ValueName
  function ValueName(m : map; idx:Longint): String;
  /// Returns the name of the kind given the index.
  ///
  /// @lib KindName
  /// @sn map:%s kindNameAtId:%s
  ///
  /// @class Map
  /// @getter KindName
  function KindName(m : map; idx:Longint): String;

  /// Returns if the map type is isometric.
  ///
  /// @lib Isometric
  ///
  /// @class Map
  /// @getter Isometric
  function Isometric(m : map): Boolean;

  
//----------------------------------------------------------------------------
// Return tile properties functions
//----------------------------------------------------------------------------
  
  /// Returns the Id of the given Tile
  ///
  /// @lib TileId
  ///
  /// @class Tile
  /// @getter id
  function TileId(t: Tile): Longint;
  /// Returns the neighbouring tile by the given tile and direction
  ///
  /// @lib TileNeightbour
  /// @sn neighbourOf:%s at:s%
  ///
  /// @class Tile
  /// @method neighbour
  function TileNeighbour(t: tile; d:Direction): Tile;
  /// Returns  the shape of a given tile
  ///
  /// @lib TileShape
  ///
  /// @class Tile
  /// @getter shape
  function TileShape(t: tile): Shape;

  

  /// Returns the center position of the given tile
  ///
  /// @lib TileCenter
  ///
  /// @class Tile
  /// @getter center
  function TileCenter(t: tile): Point2D;
  /// Return the top right position of the given tile
  ///
  /// @lib TilePosition
  ///
  /// @class Tile
  /// @getter position
  function TilePosition(t: tile): Point2D;
  /// Returns the value of a given tile at a given index
  ///
  /// @lib TileValue
  /// @sn tile:%s ValueId:%s
  ///
  /// @class Tile
  /// @method Value
  function TileValue(t: Tile; vId: Longint) : Single;
  /// Returns the kind of the given tile
  ///
  /// @lib TileKind
  ///
  /// @class Tile
  /// @getter kind
  function TileKind(t: Tile) : Longint;
  /// Returns Tile given the id of the tile
  ///
  /// @lib TileAtId
  /// @sn map:%s tileAtId:%s
  ///
  /// @class Map
  /// @method TileAt
  function TileAt(m: Map; id:Longint) : Tile; Overload;

  /// Return Tile given the top right position
  ///
  /// @lib TileAtPosition
  /// @sn map:%s tileAt:%s
  ///
  /// @class Map
  /// @overload TileAt TileAtPosition
  function TileAt(m: Map; const pos:Point2D) : Tile; Overload;

  /// Returns tile given the row and column of the tile
  ///
  /// @lib TileAtRowCol
  /// @sn map:%s tileAtRow:%s col:%s
  ///
  /// @class Map
  /// @overLoad TileAt TileAtRowCol
  /// @csn tileAtRow:%s col:%s
  function TileAt(m: Map; row, col: Longint) : Tile; Overload;
  
  
  
//----------------------------------------------------------------------------
// Set Tile procedures 
//----------------------------------------------------------------------------
  
  /// Sets the kind of a given tile
  ///
  /// @lib SetTileKind
  /// @sn setTile:%s kind:%s
  ///
  /// @class Tile
  /// @setter TileKind
  procedure SetTileKind(t : Tile; kindId : Longint);

  /// Sets the Values of a given tile at a given index
  ///
  /// @lib SetTileValue
  /// @sn setTile:%s valueId:%s value:%s
  ///
  /// @class Tile
  /// @method TileValue
  procedure SetTileValue(t : Tile; VId : Longint; value : Single); overload;

  /// Sets the values of a given tile at a given name
  ///
  /// @lib SetTileValueByName
  /// @sn map:%s tile:%s name:%s value:%s
  ///
  /// @class Tile
  /// @overload TileValue TileValueByName
  /// @csn tile:%s name:%s value:%s
  procedure SetTileValue(m : map; t :Tile; name : String; val : Single); overload;



  procedure SetTileBitmap(m:map; t : tile; layer:Longint; idx :Longint);

  /// returns an empty new map
  ///
  /// @lib NewMap
  ///
  /// @class map
  /// @constructor
  /// @csn init
  function NewMap():map;

  //======================//
  // Set map procedures   //
  //======================//
  procedure ReAssignKinds(m:map);
  
  procedure MapSetBitmapDefaultKind(m: map; const idx :longInt; kind :longInt);
  /// Removes a BitmapCellKind from the array within the given map
  ///
  /// @lib MapRemoveBitmap
  /// @sn map:%s removeIdx:%s
  ///
  /// @class Map
  /// @method RemoveBitmap
  procedure MapRemoveBitmap(m : map; const idx:Longint);
  
  /// Adds a values to map where idx1 represents kind and idx2 represents value index
  ///
  /// @lib AddMapValues
  /// @sn map:%s kindId:%s valueId:%s value:%s
  ///
  /// @class Map
  /// @method AddMapValues
  procedure AddMapValues(m : map;const  idx1,idx2 : Longint;const  val : Single);
  /// Sets the map width, height layer count, tile width, tile height and if the map is isometric.
  ///
  /// @lib MapSetDimension
  /// @sn map:%s width:%s height:%s layers:%s tileWidth:%s tileHeight:%s isometric:%s
  ///
  /// @class map
  /// @method MapSetDimension
  /// @csn width:%s height:%s layers:%s tileWidth:%s tileHeight:%s isometric:%s
  procedure MapSetDimension(m : map;  Width, height, layers, tWidth, tHeight : Longint; iso:Boolean);

  /// Allocates Default values to given tile depending on the tile's kind
  ///
  /// @lib AllocateDefaultValues
  /// @sn allocateDefaultValuesToMap:%s atTile:%s 
  ///
  /// @class Map
  /// @method AllocaDefaultValues
  procedure AllocateDefaultValues(map:Map; var tile: TileData);

  /// Reconfigures the map given new dimensions/values.
  ///
  ///@lib ReconfigureMap
  ///
  /// @class Map
  /// @method ReconfigureMap
  procedure ReconfigureMap(var m:map);

  /// Adds a single bitmap to bitmapCellKind within the map.
  ///
  /// @lib MapAddBitmap
  /// @sn map:%s addBitmap:%s
  /// 
  /// @class Map
  /// @setter AddBItmap
  procedure MapAddBitmap(m:map; filename:String);

  /// Adds a value name to the named index collection within the map
  ///
  /// @lib MapAddValue
  /// @sn map:%s addValueName:%s
  ///
  /// @class Map
  /// @setter AddValue
  procedure MapAddValue(m:map; vName:string);

  /// Adds bitmap cells to bitmap cell kind within map
  ///
  /// @lib MapAddBitmapCells
  /// @sn map:%s bitmapCellIds:%s cellRegions:%s bitmap:%s
  ///
  /// @class Map
  /// @method MapAddBitmapCells
  /// @csn bitmapCellIds:%s cellRegions:%s bitmap:%s
  procedure MapAddBitmapCells(m : map; bitmapCellIds : array of Longint; cellRegions : array of Longint; gridBitmap : Bitmap);

  /// Adds Kind Name to the numed index collection within the map.
  ///
  /// @lib AddKind
  /// @sn map:%s addKindName:%s
  ///
  /// @class Map
  /// @setter AddKind
  procedure AddKind(m:map; kname:string);
  /// Removes a value name from the named index collection and reshuffles the values
  ///
  /// @lib RemoveValueName
  /// @sn map:%s removeValueName:%s
  ///
  /// @class Map
  /// @method RemoveValue 
  procedure RemoveValue(m:map; vName:string);overload;

  /// Removes the value from the named index collection by idx and reshuffles the value
  ///
  /// @lib RemoveValueIdx
  /// @sn map:%s removeValueId:%s
  ///
  /// @class Map
  /// @overload RemoveValue RemoveValueId
  procedure RemoveValue(m:map; idx:Longint); overload;
  /// Removes the kind from the named index collection by idx and sets all tiles of removed kind to -1
  ///
  /// @lib RemoveKindIdx
  /// @sn map:%s removeKindId:%s
  ///
  /// @class Map
  /// @method RemoveKind
  procedure RemoveKind(m:map; idx:Longint); overload;
  /// Removes the kind From the named index collection by kind
  /// name and sets all tiles of removed kind to -1
  ///
  /// @lib RemoveKindName
  /// @sn map:%s removeKindName:%s
  ///
  /// @class Map
  /// @overload RemoveKind RemoveKindByName
  procedure RemoveKind(m:map; kname:string); overload;
  
  
  
//---------------------------------------------------------------------
// Resource management
//------------------------------------------------------------------
  
  /// Releases all of the map data that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllMaps();

  /// Releases the Map that have been loaded with the supplied name.
  ///
  /// @lib
  procedure ReleaseMap(name: String);
  
  /// Frees the resources used by a `Map` resource. All loaded
  /// `Map` should be freed once it is no longer needed. 
  ///
  /// @lib
  ///
  /// @class Map
  /// @dispose
  procedure FreeMap(var m: map);

  /// Loads and returns a map value. The supplied `filename` is used to
  /// locate the map file to load. The supplied `name` indicates the 
  /// name to use to refer to this map value. The `Map` can then be
  /// retrieved by passing this `name` to the `MapNamed` function. 
  ///
  /// @lib
  /// @sn loadMapNamed:%s fromFile:%s
  ///
  /// @class Map
  /// @constructor
  /// @csn initWithName:%s fromFile:%s
  function LoadMapNamed(name, filename: String): Map;

  /// Returns the filename that SwinGame uses to load to this Map data.
  ///
  /// @lib
  ///
  /// @class Map
  /// @getter Filename
  function MapFilename(m: Map): String;

  /// Returns the name that SwinGame uses to refer to this Map data. This
  /// name can be used to fetch and release this Map resource.
  ///
  /// @lib
  ///
  /// @class Map
  /// @getter Name
  function MapName(m: Map): String;

  /// Returns the `Map` that has been loaded with the specified name.
  /// This works with Map data loaded using `MapMap`.
  ///
  /// @lib
  function MapNamed(name: String): Map;

  /// Determines if SwinGame has a Map value loaded for the supplied name.
  /// This checks against all Map values loaded using `MapMap`.
  ///
  /// @lib
  function HasMap(name: String): Boolean;  
  

  implementation
  uses
    sgText, sgGraphics, sgTrace, sgResources,
    sgCamera, sgGeometry, sgImages, sgInput, sgPhysics,
    sgSprites, sgTimers, SysUtils, StrUtils, Classes,
      stringhash, sgSharedUtils, sgNamedIndexCollection, sgShared;


  //==================================================================
  //Load Map Functions
  //==================================================================
  var
      _Maps: TStringHash;
      //Add one or many name(s) to an index collection.

    Procedure AddNamesToCollection(var col: NamedIndexCollection; names: String);
    var
      i, namesLength:Longint;    
    begin
      if names = '' then exit;
      //number of names = ,'s + 1 (commas + 1)
      namesLength := CountDelimiter(names, ',') + 1;
      //WriteLn('Reading ', namesLength, ' names from ', names);
      
      for i := 1 to namesLength do
      begin
        AddName(col, ExtractDelimited(i,names,[',']));
      end;
    end;


   

    procedure AllocateDefaultValues(map:Map; var tile: TileData);
    var
      kindIdx, i : Longint;
    begin
      kindIdx := tile.kind;
      SetLength(tile.values, NameCount(map^.valueIds));
      ZeroArray(tile.values);
      
      if (kindIdx < 0) or (kindIdx > High(map^.MapDefaultValues)) then exit;

      //Allocate space for the values
      //SetLength(tile.values, Length(map^.MapDefaultValues[kindIdx]));
      
      for i := 0 to High(tile.values) do
      begin
        //if i > High(map^.MapDefaultValues[kindIdx]) then tile.Values[i] := 0;
        tile.values[i] := map^.MapDefaultValues[kindIdx, i];
      end;
    end;


  function DoLoadMap(filename:string; name:String): Map;

  type
    SpecificValuesLoader = record
      row:      Longint;
      col:      Longint;
      name:     string;
      value:    Single;
    end;



  var
    tempValues:       Array of SpecificValuesLoader;  // contains the row,col, name and value
    textFile:         Text;                           // text file that is loaded
    id:               String;                         // id for the line processor to identify how to handle the data
    data,line:        String;                         // line is read then seperated into id and data.
    lineno:           Longint;                        // line number for exceptions

    procedure AllocateSpecificValues();
    var
    i: Longint;
    begin
      for i := low(tempValues) to high(tempValues) do
      begin
      
        SetTileValue(result, TileAt(result,tempValues[i].row, tempValues[i].col), tempValues[i].name, tempValues[i].value);
        //writeln('length of values within tiles', length(result^.Tiles[3,3].values));
        //writeln(tempValues[i].row,',',tempValues[i].col,',',tempValues[i].name,',',tempValues[i].value);

      end;      
    end;

  // use data processed to set tile properties.
    Procedure SetTiles();
    var
      row,col,id : Longint;
    begin
      id:=0;
      SetLength(result^.Tiles, result^.MapHeight, result^.MapWidth); //set lengths of tiles
      for row:=low(result^.Tiles) to high (result^.Tiles) do
        for col:=low(result^.Tiles[row]) to high(result^.Tiles[row]) do
        begin
          SetLength(result^.Tiles[row,col].TileBitmapCellKind, result^.MapLayer); // set length of bitmapcells per tile.
          result^.Tiles[row,col].TileID:=id;
          id+=1;
        end;
    end;
    
    // ADD tile procedure*************
    //
    // Reads the ti: data
    // Reading the regions from the file

    procedure AddTile();

    var
      bitmapIdxs: Array of Longint;
      layer, i,rowNum:Longint;
      current : Longint;
    begin
      rowNum:= 0;
      if Length(result^.Tiles) = 0 then ReconfigureMap(result);
      layer:=MyStrToInt(data, false);
      
      while RowNum < result^.MapHeight do
      begin
        ReadLn(textFile, data);
        lineNo += 1;
        bitmapIdxs := ProcessRange(data);
        
        if Length(bitmapIdxs)<>result^.MapWidth then
          RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Length of bitmapIdxs is ' + IntToStr(Length(bitmapIdxs)) + ' but should be ' + IntToStr(result^.MapWidth));

        for i:=low(bitmapIdxs) to high(bitmapIdxs) do
        begin
        result^.Tiles[rowNum, i].TileBitmapCellKind[layer] := nil;
          current := bitmapIdxs[i];
          SetLength(result^.Tiles[rowNum, i].TileBitmapCellKind, result^.MapLayer);
          if current <> -1 then // used -1 as no bitmap.
          begin
            //result^.Tiles[rowNum, i].TileBitmapCellKind[layer].Bmap    := bitmapCellArray[current].Bmap;
            //result^.Tiles[rowNum, i].TileBitmapCells[layer].Cell    := bitmapCellArray[current].Cell;
            //result^.Tiles[rowNum, i].TileBitmapCells[layer].KindIdx := bitmapCellArray[current].KindIdx;
            result^.Tiles[rowNum, i].TileBitmapCellKind[layer] := @result^.BitmapCellKind[current];
            
            if layer = 0 then
            begin
             //writeln('adding kind');
              result^.Tiles[rowNum, i].kind := result^.BitmapCellKind[current].KindIdx;
            end;
            //result^.Tiles[row,col].Values:= result^.MapDefaultValues
          end
          else if layer = 0 then
          result^.Tiles[rowNum, i].kind := -1
          else
          result^.Tiles[rowNum, i].TileBitmapCellKind[layer]:=nil;

        end;
        RowNum += 1;
      end;
    end;

    //sets up positions of each tile
    procedure ProcessTiles();
    var
      row,col: Longint;
    begin
      for row:=low(result^.Tiles) to high(result^.Tiles) do
      begin
        for col:=low(result^.Tiles[row]) to high(result^.Tiles[row]) do
          begin

            //kind
            AllocateDefaultValues(result, result^.Tiles[row,col]);
          end;
      end;
          ReconfigureMap(result);
          AllocateSpecificValues();
    end;



     // Adds default values depending on kind
    procedure AddDefaultValue(data:String);
    var
    defaultValues:SingleArray;
    kind,i: Longint;
    begin
      kind:=MyStrToInt(ExtractDelimited(1,data,[',']),false);
      
      defaultValues:=ProcessFloatRange(ExtractDelimitedWithRanges(2,data));
      //add exception if length of default value <> length of valueIds.naresultes.
      if ((length(result^.MapDefaultValues) = 0) AND (NameCount(result^.kindIds) > length(result^.MapDefaultValues))) then
      begin
       //sets length of default value to length of kinds and nuresultber of naresultes of value
          SetLength(result^.MapDefaultValues, NameCount(result^.KindIds), NameCount(result^.valueIds));
          ZeroArray(result^.MapDefaultValues);
      end;
      for i:=low(result^.MapDefaultValues[kind]) to high(result^.MapDefaultValues[kind]) do
      begin
        result^.MapDefaultValues[kind, i] := defaultValues[i];
      end; 
    end;
  


    //Maps kinds to the bitmap
    procedure MapBitmapToKind(data:string);
    var
    i: Longint;
    kindIds:LongintArray;
    begin
      kindIds:=ProcessRange(ExtractDelimitedWithRanges(1,data));
      if length(kindIds) = length(result^.BitmapCellKind) then
        begin
          for i:=low(result^.BitmapCellKind) to high(result^.BitmapCellKind) do
          begin
          result^.BitmapCellKind[i].KindIdx := kindIds[i];
          end;
        end
        else
        RaiseException('The number of kinds passed in does not match the number of bitmaps');
    end;

    procedure LoadSpecificValue(data:String);
    var
    row,col:Longint;
    val:single;
    name:String;
    begin
      SetLength(tempValues, length(tempValues)+1);
      row:=MyStrToInt(ExtractDelimited(1, data, [',']), false);
      col:=MyStrToInt(ExtractDelimited(2, data, [',']), false);
      name:=ExtractDelimited(3, data, [',']);
      val:=StrToSingle(ExtractDelimited(4, data, [',']));
      tempValues[high(tempValues)].row := row;
      tempValues[high(tempValues)].col := col;
      tempValues[high(tempValues)].name := name;
      tempValues[high(tempValues)].value := val;

    end;
    //process lines that deals with value
    procedure ProcessValueLineData();
    begin
      case LowerCase(id)[3] of
        
        // Value Name
        'n':  AddNamesToCollection(result^.valueIds, data);
              
        // Value Default
        'd':  AddDefaultValue(data);

      end;
    end;

    //process lines that deals with kind
    procedure ProcessKindLineData();
    begin
      case LowerCase(id)[3] of
      'n': AddNamesToCollection(result^.kindids, data);
      'i': MapBitmapToKind(data);
      end;
    end;
    
      // Reads id and data for
    // mw = map width
    // mh = map height
    // ml = map layers
    // mhc = map highlight color
    // mvn = map value name
    procedure ProcessMapLineData();
    begin
      case LowerCase(id)[2] of
        //map width
        'w':  result^.MapWidth  := MyStrToInt(data, false);
        
        //  map height
        'h':  if length(id)=2 then
                result^.MapHeight := MyStrToInt(data, false) 
              else if LowerCase(id)[3] = 'c' then result^.mapHighlightcolor:= RGBAColor(
                StrToUByte(ExtractDelimited(1, data, [','])),
                StrToUByte(ExtractDelimited(2, data, [','])),
                StrToUByte(ExtractDelimited(3, data, [','])),
                StrToUByte(ExtractDelimited(4, data, [','])));

        // number of layers        
        'l':  result^.MapLayer  := MyStrToInt(data, false);

        //Isometric 
        'i':  if( lowerCase(data) = 'true') then
              begin
                result^.Isometric := true;
              end
              else
              begin
                result^.Isometric := false;
              end;
              
        //kind  
        'k':  ProcessKindLineData();      
        //Value
        'v':  ProcessValueLineData();
        else
        begin
          RaiseException('Error at line' + IntToStr(lineNo) + 'in map' + filename + '. error with id: ' + id + '. Id not recognized.');
          exit;
        end;
      end;
    end;

    procedure AddSpecificKind(data: string);
    var
    row,col,kidx:longInt;
    begin
      row := MyStrToInt(ExtractDelimited(1, data, [',']));
      col := MyStrToInt(ExtractDelimited(2, data, [',']));
      kidx := MyStrToInt(ExtractDelimited(3, data, [',']));
      SetTileKind(TileAt(result,row,col), kidx);
    end;
    // processes lines regarding tiles
    // tw = tile width
    // th = tile height
    // tl = tile layer
    // tb = tile bitmap
    // to = tile offset
    procedure ProcessTileLineData();
    begin
      case LowerCase(id)[2] of
        'w': result^.TileWidth  := MyStrToInt(data, false);        //  tile width
        'h': result^.TileHeight := MyStrToInt(data, false);      // tile height
        'l': AddTile();
        'b': MapAddBitmap(result, data);
        'v':LoadSpecificValue(data);
        'k':AddSpecificKind(data);
      end;
    end;
    //Process line procedure*************
    procedure ProcessLine();
    var
    bitmapCellIds : array of Longint;
    cellRegions   : array of Longint;
    gridBitmap    : Bitmap;
    cellWidth,
    cellHeight,
    cellRows,
    cellCols,
    cellCount     : Longint;
    begin
      // Split line into id and data
      id   := ExtractDelimited(1, line, [':']);
      data := ExtractDelimited(2, line, [':']);
      // Verify that id is two chars
      if ((Length(id) <> 2) AND (Length(id) <> 3)) then
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. This id should contain 2 or 3 characters.');
        exit;
      end;
      // Process based on id
      case LowerCase(id)[1] of // in all cases the data variable is read
        'm': ProcessMapLineData();
        't': ProcessTileLineData();
        'g':
        begin
          bitmapCellIds   := ProcessRange(ExtractDelimitedWithRanges(1,data));
          cellRegions     := ProcessRange(ExtractDelimitedWithRanges(2,data));
          gridBitmap      := LoadBitmap(ExtractDelimitedWithRanges(3,data));
          cellWidth       := MyStrToInt(ExtractDelimitedWithRanges(4,data),false);
          cellHeight      := MyStrToInt(ExtractDelimitedWithRanges(5,data),false);
          cellRows        := MyStrToInt(ExtractDelimitedWithRanges(6,data),false);
          cellCols        := MyStrToInt(ExtractDelimitedWithRanges(7,data),false);
          cellCount       := MyStrToInt(ExtractDelimitedWithRanges(8,data),false);
          BitmapSetCellDetails(gridBitmap, cellWidth, cellHeight, cellCols, cellRows, cellCount);
          MapAddBitmapCells(result, bitmapCellIds, cellRegions, gridBitmap);
        end;
        else
        begin
          RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. id not recognized.');
          exit;
        end;
      end;
    end;
    

  begin
    //load map starts here
    if not FileExists(filename) then
    begin
      filename := PathToResource(filename, MapResource);
      if not FileExists(filename) then
      begin
        RaiseException('Unable to locate sound m ' + filename);
        exit;
      end;
    end;
    result := NewMap();
    result^.filename := filename;
    result^.name := name;
    Assign(textFile, filename);
    lineNo:=0;
    
    //moves cursor to 1,1
    Reset(textFile);
    ReadLn(textFile,line);
    lineNo := lineNo + 1;
    if line <> 'Map Loader #v0.1' then
    begin
      RaiseException('Error: Map File Corrupted ' + filename);
      exit;
    end;
    while not EOF(textFile) do
    begin
      ReadLn(textFile, line);
      lineNo := lineNo + 1;
      //writeln(lineNo);
      line   := Trim(line);
      if Length(line) = 0 then continue;  //skip emapty lines
      if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
      ProcessLine();
    end;
    //writeln('PrototypeAdded');
    ProcessTiles();
    //writeln('TileProcessed')  ;
    close(textFile);
  end;
  
  procedure MapRangeFromRect(r:Rectangle; map:Map; out startX, startY, endX, endY:Longint  );
  begin
    //WriteLn('Getting range for ', RectangleToString(r));
    // Calculate the tiles that are on the screen - only draw these
    if map^.TileHeight or map^.TileWidth = 0 then exit;
    
    // Trunc so that 210 / 50 becomes 4 - i.e. the index of the 5th tile... (saves - 1)
    startY := Trunc(RectangleTop(r) / (map^.TileHeight - map^.TileStagger.Y));
    startX := Trunc(RectangleLeft(r) / map^.TileWidth);
    
    // equation is (bottom - stagger) / (height - stagger) = redone with + 1
    endY   := Trunc(RectangleBottom(r) / (map^.TileHeight - map^.TileStagger.Y)) + 1;
    endX   := Trunc((RectangleRight(r) + map^.TileStagger.X) / map^.TileWidth);

    // Adjust the end and start to be in range of the array
    if endY >= (map^.MapHeight) then endY := map^.MapHeight-1;
    if endX >= (map^.MapWidth) then endX := map^.MapWidth-1;
    if startX >= (map^.MapWidth) then startX := map^.MapWidth-1;
    if startY >= (map^.MapHeight) then startY := map^.MapHeight-1;
    
    if endY <0 then endY := 0;
    if endX <0 then endX := 0;
    
    if startY < 0 then startY := 0;
    if startX < 0 then startX := 0;   
    
    //WriteLn('result ', startX, ':', startY, ' ', endX, ':', endY);
  end;

  procedure PushMapClip(map:Map; startCol, startRow, endCol, endRow : Longint; offset :Vector);
  var
    rowCount, colCount: Longint;
    width, height : Longint;
    pt: Point2D;
  begin
    //WriteLn('cols: ', startCol, ' to ', endCol);
   // WriteLn('rows: ', startRow, ' to ', endRow);
    rowCount := (endRow - startRow)+1;
    colCount := (endCol - startCol)+1;
    //writeln('rowcount: ',rowCount, ' ','colcount: ',ColCount);
    // # of columns * tilewidth - stagger X for even rows.
    width := Round ( (colCount * map^.TileWidth) - ( ( (startRow+1) mod 2)* (map^.TileStagger.X)));
    // # of row ^ tileheight - number of rows * staggerY  -- in conjunction with if rowcount = 20...
    height := RoundInt(((rowCount) * map^.TileHeight) - ((rowCount)*(map^.TileStagger.Y)));
    
   // writeln('width: ',width, ' ','height: ',height);
    if not assigned(TileAt(map, startRow, startCol)) then exit;
    pt := PointAdd(TileAt(map, startRow, startCol)^.position, offset);
    pt := PointAdd(pt, InvertVector(CameraPos()));
    //the last row doesnt clip at 20 because the camera has not reached the next row.
    if rowcount = map^.MapHeight then height-=RoundInt(map^.TileStagger.Y);
    //apply stagger x only on the first column and even values.
    if (startCol = 0) and  not(startRow Mod 2=0) then pt.X += map^.TileStagger.X;
    //apply stagger to top of the map only.
    if (startrow = 0) then pt.Y += map^.TileStagger.Y;
    //WriteLn('Clip At: ', PointToString(pt));
    PushClip(RectangleFrom(pt.x, pt.y, width+1 ,height+1)); // +1 because in grid mode the line is drawn 1 px outside the shape.
    PushClip(RectangleFrom(offset.x, offset.y, ScreenWidth,ScreenHeight)); // +1 because in grid mode the line is drawn 1 px outside the shape.
  end;

  //checks if tile is Selected.
  function TileSelected(map:Map; tile:Tile):Boolean;
  var
  k:Longint;
  begin
    for k:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
      begin
        if map^.SelectedTiles[k] = tile^.TileID then
        begin
          result:=True;
          exit;
        end
      end;
    result :=False;
  end;


  //procedure to deselect a tile. (includes removing from array and reducing array length)
  procedure Deselect(map:Map; tile:Tile);
  var
  l,m : Longint;
  begin
    for l:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
      if (map^.SelectedTiles[l] = tile^.TileID) then
        begin
          for m:=l to high(map^.SelectedTiles) do
            begin
              map^.Selectedtiles[m]:= map^.SelectedTiles[m+1];
            end;
          SetLength(map^.SelectedTiles, length(map^.SelectedTiles)-1);
        end;
  end;
  
  //procedure to highlight one tile.
  
  procedure HighlightTile(highlightedTile: Tile; offset: Vector); OverLoad;
  begin
    if not assigned(highlightedTile) then exit;
    DrawShapeAsLineStrip(screen, highlightedTile^.TileShape, false, PointAdd((CameraPos*-1),offset));
  end;

  procedure HighlightTile(highlightedTile: Tile); OverLoad;
  begin
    HighlightTile(highlightedTile, VectorTo(0,0));
  end;

  
  //updates whether a tile is highlighted.
  procedure UpdateHighlight(map:Map);
  var
  i:Longint;
  begin
    for i:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
            HighlightTile(TileAt(map, map^.SelectedTiles[i]));
  end;
  
  //updates whether a tile should be selected or deselected.
  procedure UpdateSelect(map:Map);
  var
    //startCol, startRow, endCol, endRow:Longint;
    t: Tile;
  begin
    t := TileAt(map, ToWorld(MousePosition()));
    if not assigned(t) then exit;
    if not TileSelected(map, t) then
    begin
      SetLength(map^.SelectedTiles, Length(map^.SelectedTiles)+1);
      map^.SelectedTiles[high(map^.SelectedTiles)]:=t^.TileId;
     // writeln('selected: ',t^.TileId);
    end
    else
    begin
    //writeln('deselecting: ',t^.TileId);
      Deselect(map, TileAt(map, t^.TileId));
    end;
  end;

  
  procedure DoDrawMapLayer(map: Map; offset:Vector; layer, startCol, startRow, endCol, endRow:Longint); overload;
  var
    col, row : Longint;
   // dir:Direction;
  begin
    if not assigned(map) or (map^.MapWidth = 0) or (map^.MapHeight = 0) then exit;
    
    // Loop through all the layers + and additional one for debug info

      for row:=startRow to endRow do
      begin
        for col:=startCol to endCol do
        begin
          if map^.Tiles[row,col].TileBitmapCellKind[layer] <> nil then
            if map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap <> nil then
            begin
             // writeln(HexStr(map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap));
                DrawCell(map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap, map^.Tiles[row,col].TileBitmapCellKind[layer]^.Cell, PointAdd(map^.Tiles[row,col].Position,offset));
            end;
        end; // end of col in row
      end; // end of row
  end;

  
  procedure DrawMapLayer(map: Map; offset:Vector; layer:Longint); overload;
  var
    startRow, startCol, endRow, endCol : Longint;
  begin
    if not assigned(map) or (map^.MapWidth = 0) or (map^.MapHeight = 0) then exit;
    MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);
    PushMapClip(map, startCol, startRow, endCol, endRow, offset);
    DoDrawMapLayer(map, offset,layer, startCol, startRow, endCol, endRow );
    PopClip(); // push clip pushes 2 clips
    PopClip();

  end;

  procedure DrawMap(map: Map; offset:vector); overload;
  var
    layer : longint;
  begin
    for layer := 0 to LayerCount(map)-1 do
    begin
      DrawMapLayer(map, offset, layer);
    end;
  end;
  
  procedure DrawMap(map: Map); overload;
  begin
    DrawMap(map, VectorTo(0,0));
  end;

  procedure DrawMapDebug(map: map); 
  var
  dir: Direction;
  row,col,startRow, startCol, endRow, endCol : Longint;
  begin
    if not assigned(map) or (map^.MapWidth = 0) or (map^.MapHeight = 0) then exit;
    MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);
    for row:=startRow to endRow do
    begin
      for col:=startCol to endCol do
      begin
  //Show debug information over last layer
        if TileSelected(map, @map^.Tiles[row,col]) then
        begin
          //Draw debug information
          DrawText(IntToStr(col) + ':' + IntToStr(row), map^.mapHighlightcolor, map^.Tiles[row,col].Position);
          DrawText(IntToStr(map^.Tiles[row,col].kind), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 10);
          if length(map^.Tiles[row,col].values) <> 0 then
          DrawText(FloatToStr(map^.Tiles[row,col].values[0]), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 20);

          for dir:=low(map^.Tiles[row,col].SurroundingTiles) to high(map^.Tiles[row,col].SurroundingTiles) do
          begin
            if not assigned(map^.Tiles[row,col].SurroundingTiles[dir]) then continue
            else
            DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[dir]^.center);
          end;
        end;
      end;
    end;
        UpdateHighlight(map);
  end;

  procedure DrawMapGrid(m:map; offset:Vector); overload;
  var
    row,col,startRow, startCol, endRow, endCol : Longint;
  begin
    if not assigned(m) or (m^.MapWidth = 0) or (m^.MapHeight = 0) then exit;
    MapRangeFromRect(CameraScreenRect(), m, startCol, startRow, endCol, endRow);

    PushMapClip(m, startCol, startRow, endCol, endRow,offset);
    for row := startRow to endRow do
    begin
      for col := startCol to endCol do
      begin
        HighlightTile(@m^.Tiles[row,col], offset);
      end;
    end;
        PopClip();//push map clip adds 2 clips
    PopClip();
  end;

  procedure DrawMapGrid(m:map); overload;
  begin
    DrawMapGrid(m,VectorTo(0,0));
  end;




//---------------------------------------------------------------------------
// SAVE MAP
//---------------------------------------------------------------------------
  Procedure SaveMap(m:Map; filename:String);
  type
  cellkind = record
    cell      : Longint;
    kindIdx      : Longint;
    end;

  cellKindArray = array of cellkind;
  
  BitmapCells = record
    bmp       : Bitmap;
    Cellkinds : cellKindArray;
    startIdx  : Longint;
    kindIdx   : Longint;
  end;
  var
  output: text;
  bitmapCellsArray:  Array of BitmapCells;
    procedure _CheckAddBitmap(bmp: Bitmap; cell,kindIdx: Longint);
    var
      i,j:Longint;
    begin
     // WriteLn('Checking ', HexStr(bmp), ' ', cell, ' kind ', kindIdx);
      
      if not assigned(bmp) then exit;
      // For all of the cells we have so far
      for i:=low(bitmapCellsArray) to high(bitmapCellsArray) do
      begin
        //if this is the bitmap we are after...
        if (bitmapCellsArray[i].bmp = bmp) then
        begin
          //check if we have the cell...
          for j:=low(bitmapCellsArray[i].CellKinds) to high(bitmapCellsArray[i].CellKinds) do
          begin
            //if found... exit out of proc
            if bitmapCellsArray[i].CellKinds[j].cell = cell then exit;
          end;
          // Need to add cellkind to bitmap
          SetLength(bitmapCellsArray[i].CellKinds, length(bitmapCellsArray[i].CellKinds) + 1);
          bitmapCellsArray[i].CellKinds[high(bitmapCellsArray[i].CellKinds)].cell := Cell;
          bitmapCellsArray[i].CellKinds[high(bitmapCellsArray[i].CellKinds)].kindIdx := kindIdx;
          
          exit;
        end;
      end;
      
      // Got here because there was no matching bitmap... so add it
      SetLength(bitmapCellsArray, length(bitmapCellsArray) + 1);
     // writeln(Length(bitmapCellsArray));
      SetLength(bitmapCellsArray[high(bitmapCellsArray)].CellKinds, 1);
      bitmapCellsArray[high(bitmapCellsArray)].CellKinds[0].Cell := cell;
      bitmapCellsArray[high(bitmapCellsArray)].Bmp := bmp;
      bitmapCellsArray[high(bitmapCellsArray)].CellKinds[0].KindIdx := kindIdx;
      
     // writeln('cur '+bitmapName(bitmapCellsArray[i].bmp),' passed '+bitmapName(bmp));
    end;

    procedure _SetStartIdx();
    var
    i:Longint;
    begin
      if length(bitmapCellsArray) = 0 then exit;
      bitmapCellsArray[0].startIdx:=0;
      for i:= (low(bitmapCellsArray)+1) to high(bitmapCellsArray) do
      begin
        bitmapCellsArray[i].startIdx := (bitmapCellsArray[i-1].startIdx + length(bitmapCellsArray[i-1].CellKinds));
      end;
    end;


    function _IndexOfMapCellKind(ptr:BitmapCellKindPtr): Longint;
    var
    i:Longint;
    begin
    result := -1;
    if ptr = nil then exit;
    for i := low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
    begin
      if ptr = @m^.BitmapCellKind[i] then
      result:= i;
    end;
    
    end;
    
    procedure _WriteBitmapInfo();
    var
    i,j:Longint;
    endIdx : longint;
    cells : LongintArray;
    currentBitmap : BitmapCells;
    cellrange:string;
    begin
      for i:=low(bitmapCellsArray) to high(bitmapCellsArray) do
      begin
        currentBitmap := bitmapCellsArray[i];
        if length(currentBitmap.CellKinds) = 1 then
        begin
          writeln(output, 'tb:',currentBitmap.startIdx,','+BitmapName(currentBitmap.bmp));
        end
        else
        begin

          //make cell into LongintArray of cells
          SetLength(cells,length(currentBitmap.CellKinds));
          for j := low(currentBitmap.CellKinds) to high(currentBitmap.CellKinds) do
          begin
            cells[j] := currentBitmap.CellKinds[j].cell;
          end;
         cellrange := LongintArrayToRange(cells);
          endIdx:=bitmapCellsArray[i].startIdx+length(bitmapCellsArray[i].CellKinds)-1;
        writeln(output,'gb:[',bitmapCellsArray[i].startIdx,'-',endIdx,'],',cellrange,
              ',',BitmapName(bitmapCellsArray[i].bmp),
              ',',IntToStr(BitmapCellWidth(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellHeight(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellRows(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellColumns(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellCount(bitmapCellsArray[i].bmp)));
        end;
      end;
    end;

    
    
    procedure _SaveMapBitmaps();
    var
      row,col,bmpIdx:Longint;
      current :Array of BitmapCellKind;
    begin
      for row:=low(m^.Tiles) to high(m^.Tiles) do
      begin
        for col:=low(m^.Tiles[row]) to high(m^.Tiles[row]) do
        begin
          current := m^.BitmapCellKind;

          
          for bmpIdx := low(current) to high(current) do
          begin
            _CheckAddBitmap(current[bmpIdx].Bmap, current[bmpIdx].cell, current[bmpIdx].KindIdx);
          end;
        end;
      end;
      _SetStartIdx();
      _WriteBitmapInfo();
    end;

  procedure _WriteKindBitmap();
  var
    i,j : Longint;
    mki : string;
  begin
    if length(bitmapCellsArray) = 0 then exit;
    mki := 'mki:[';
    
    for i := Low(bitmapCellsArray) to High(bitmapCellsArray) do
    begin
      for j := Low(bitmapCellsArray[i].CellKinds) to High(bitmapCellsArray[i].CellKinds) do
      begin
        if length(mki) > 5 then
          mki += ',';
        mki += IntToStr(bitmapCellsArray[i].CellKinds[j].kindIdx);
      end;
    end;
    mki+=']';
    writeln(output,mki);
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

  procedure _WriteKindName();
  var
  mkn : string;
  begin
    mkn:='mkn:'+NamedIndexCollectionNameList(m^.kindIds);
    writeln(output, mkn);
  end;

  procedure _WriteValueName();
  var
  mvn : String;
  begin
    mvn:='mvn:'+NamedIndexCollectionNameList(m^.valueIds);
    writeln(output, mvn);
  end;
  procedure _WriteTileLayers();
  var
    ids : LongintArray;
    row,col, layer : Longint;
    currentTile: TileData;
  begin
    SetLength(ids, m^.MapWidth);
    for layer := 0 to m^.MapLayer-1 do
    begin
      writeln(output, 'tl: ', IntToStr(layer));
      for row := Low(m^.Tiles) to High(m^.Tiles) do
      begin
        for col := Low(m^.Tiles[row]) to High(m^.Tiles[row]) do
        begin
          currentTile := m^.Tiles[row,col];
          //add indexes to ids.
          ids[col]:=_IndexOfMapCellKind(currentTile.TileBitmapCellKind[layer]);
        end; // end col
        writeln(output, LongintArrayToRange(ids));
      end; // end row
    end; // end layer
  end;

  procedure _WriteTileKind();
  var
    row,col : Longint;
    currentTile : TileData;
  const
    LAYER = 0;
  begin
    for row := low(m^.Tiles) to high(m^.Tiles) do
      for col := low(m^.Tiles[row]) to high(m^.Tiles[row])do
      begin
        currentTile := m^.Tiles[row,col];
       //writeln(currentTile.TileBitmapCellKind[LAYER]^.kindIdx);
        if not Assigned(currentTile.TileBitmapCellKind[LAYER]) then
        begin
          if currentTile.Kind <> -1 then writeln(output,'tk:',row,',',col,',',currentTile.Kind);

        end
        else if currentTile.Kind <> currentTile.TileBitmapCellKind[LAYER]^.kindIdx then writeln(output,'tk:',row,',',col,',',currentTile.Kind);

      end;
  end;

  procedure _WriteDefaultValues();
  var
  i : Longint;
  currentArr : SingleArray;
  begin
    for i := low(m^.MapDefaultValues) to high(m^.MapDefaultValues) do
    begin
      currentArr := m^.MapDefaultValues[i];
      if not assigned(currentArr) then break;
      writeln(output,'mvd:',i,',',SingleArrayToRange(currentArr));
    end;
  end;

  procedure _WriteTileValues();
  var
  row, col, vId: Longint;
  currentTile : TileData;
  currentValue : Single;
  begin
    for row := low(m^.Tiles) to high(m^.Tiles) do
      for col := low(m^.Tiles[row]) to high(m^.Tiles[row])do
      begin
        currentTile := m^.Tiles[row,col];
        for vId := low(currentTile.Values) to high(currentTile.Values) do
        begin
          currentValue := currentTile.Values[vId];
          if currentTile.kind <> -1 then
          begin
            if currentValue <> m^.MapDefaultValues[currentTile.Kind,vId] then
              writeln(output,'tv:',row,',',col,',',NameAt(m^.ValueIds,vId),',',currentValue);
          end;
        end;
      end;
  end;

  begin
    SetLength(bitmapCellsArray, 0);
    
    Assign(output, filename);
    rewrite(output);
    writeln(output, 'Map Loader #v0.1');
    writeln(output, '// mw : width of map');
    writeln(output, 'mw:'+IntToStr(m^.MapWidth));
    writeln(output, '// mh : height of map');
    writeln(output, 'mh:'+IntToStr(m^.MapHeight));
    writeln(output, '// ml : Number of map Layers');
    writeln(output, 'ml:'+IntToStr(m^.MapLayer));
    writeln(output, '//mhc : r,g,b,a (byte)');
    writeln(output, 'mhc:'+ColorToString(m^.MapHighlightcolor));
    writeln(output, '// tw : Width of tile');
    writeln(output, 'tw:'+IntToStr(m^.TileWidth));
    writeln(output, '// th : Height of tile');
    writeln(output, 'th:'+IntToStr(m^.TileHeight));
    writeln(output, '// mi : isometric:true or false');
    writeln(output, 'mi:',m^.Isometric);
    writeln(output, '// Tile Bitmaps');
    writeln(output, '//');
    writeln(output, '// Format:');
    writeln(output, '// tb: [tile index], [bitmap filename]');
    writeln(output, '// gb: [tile index range], [cell range], [bitmap filename], [cell width], [cell height] [cells Row] [cells columns] [cell count]');
    _SaveMapBitmaps();
    writeln(output, '//');
    writeln(output, '// Tile -> Kind Mapping');
    writeln(output, '// Format:');
    writeln(output, '// mkn: [name1], [name2]...');
    writeln(output, '// mki: [kind index ids - position of values match bitmap index]');
    _WriteKindBitmap();
    _WriteKindName();
    _WriteTileLayers();
    writeln(output, '//');
    writeln(output, '// Map Kinds');
    writeln(output, '//');
    writeln(output, '// Format:');
    writeln(output, '// // TK: [row],[col], [kind index]');
    _WriteTileKind();
    writeln(output, '//');
    writeln(output, '// Map Values');
    writeln(output, '//');
    writeln(output, '// Format:');
    writeln(output, '// - Define the names');
    writeln(output, '// mvn: [name list]');
    writeln(output, '// - Define the default values');
    writeln(output, '// mvd: [kind idx], [range of default values - same number/order as names]');
    writeln(output, '//');
    _WriteValueName();
    _WriteDefaultValues();
    writeln(output, '//');
    writeln(output, '// Tile Values');
    writeln(output, '//');
    writeln(output, '// Format:');
    writeln(output, '// tv: [row index], [col index], [value name], [value''s value]');
    writeln(output, '//');
    _WriteTileValues();

    close(output);
  end;
  



//====================================== //
//           collision codes            //
//=====================================//

    //gets potential collision and makes a rect out of it.
  function GetPotentialCollisions(map: Map; s: Sprite): Rectangle;
  var
    startPoint, endPoint: Rectangle;
    startX, startY, endX, endY: Longint;
  begin
    if map^.MapWidth = 0 or map^.MapHeight then exit;
    with map^ do
    begin
      startPoint := RectangleFrom(
        RoundInt( ((s^.position.x - s^.velocity.x) / TileWidth) - 1) * TileWidth,
        RoundInt( ((s^.position.y - s^.velocity.y) / tileheight) - 1) * tileheight,
        (RoundInt( SpriteWidth(s) / TileWidth) + 2) * TileWidth,
        (RoundInt( SpriteHeight(s) / tileheight) + 2) * tileheight
      );
      endPoint := RectangleFrom(
        RoundInt(((s^.position.x + SpriteWidth(s)) / TileWidth) - 1) * TileWidth,
        RoundInt(((s^.position.y + SpriteHeight(s)) / tileheight) - 1) * tileheight,
        (RoundInt(SpriteWidth(s) / TileWidth) + 2) * TileWidth,
        (RoundInt(SpriteHeight(s) / tileheight) + 2) * tileheight
      );
    end; // with

    //Encompassing Rectangle
    if startPoint.x < endPoint.x then
    begin
      startX := RoundInt(startPoint.x);
      endX := RoundInt(endPoint.x + endPoint.width);
    end
    else
    begin
      startX := RoundInt(endPoint.x);
      endX := RoundInt(startPoint.x + startPoint.width);
    end;

    if startPoint.y < endPoint.y then
    begin
      startY := RoundInt(startPoint.y);
      endY := RoundInt(endPoint.y + endPoint.height);
    end
    else
    begin
      startY := RoundInt(endPoint.y);
      endY := RoundInt(startPoint.y + startPoint.height);
    end;
    
    // -1 of width and height so as not to project into next tiles (200/50 = 4... the 5th tile, we want 199/50 = 3... the 4th tile)
    result := RectangleFrom(startX, startY, endX - startX - 1, endY - startY - 1);
    //drawRectangle(colorpink, RectangleFrom(startX, startY, endX - startX, endY - startY));

    //Debug Info
    //DrawRectangle(ColorYellow, startPoint);
    //DrawRectangle(ColorWhite, endPoint);
    //DrawRectangle(ColorGreen, result);
  end;

// outs the tile X and Y that the sprite has collided with  map and result is a Boolean if it did collide.
  function SpriteHasCollidedWithTile(map: Map; k: Longint; s: Sprite; out collidedX, collidedY: Longint): Boolean; overload;
  var
    y, x, dy, dx, i, j, initY, initX: Longint;
    yRange, xRange: Longint;
    xStart, yStart, xEnd, yEnd: Longint;
    rectSearch: Rectangle;
    side: CollisionSide;
  begin
    result := false;
    if map = nil then exit;
    if s = nil then exit;
    
    //makes a rect of bounding area to search so that it doesn't need to search the whole map.
    
    rectSearch := GetPotentialCollisions(map, s);
    
    //makes range out of rect.
    //WriteLn('Creating search rectangle for sprite at ', RectangleToString(SpriteCollisionRectangle(s)));
    //WriteLn('                            search rect ', RectangleToString(rectSearch));
    //WriteLn('                            heading     ', PointToString(SpriteVelocity(s)));
    MapRangeFromRect(rectSearch, map, xStart, yStart, xEnd, yEnd);

    
    //checks which side to use to check for collision
    side := SideForCollisionTest(s^.velocity);
    
    // Use the side to determine search order, allowing the tiles to be 
    // checked in a sensible order based on movement of sprite
    case side of
      TopLeft:      begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      TopRight:     begin dy := 1;  dx := -1; initY := yStart;  initX := xEnd;    end;
      BottomLeft:   begin dy := -1; dx := 1;  initY := yEnd;    initX := xStart;  end;
      BottomRight:  begin dy := -1; dx := -1; initY := yEnd;    initX := xEnd;    end;
      Top:          begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      Bottom:       begin dy := -1; dx := 1;  initY := yEnd;    initX := xStart;  end;
      Left:         begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      Right:        begin dy := 1;  dx := -1; initY := yStart;  initX := xEnd;    end;
      else          begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
    end;
    
    // Determine the number of tiles to check in the x and y directions
    yRange := yEnd - yStart;
    xRange := xEnd - xStart;
    
    //writeln ('StartY: ', yStart,  ', endY:', yEnd);
    //writeln ('StartX: ', xStart,  ', endX:', xEnd);
    
    with map^ do 
    begin
      // For i = the tiles within the y range (inclusive of start/end)
      for i := 0 to yRange do
      begin
        y := initY + i * dy;
        
        // for j = the tiles within the x range (inclusive of start/end)
        for j := 0 to xRange do
        begin
          x := initX + j * dx;
          
          // Writeln ('  Checking tile: ', x,  ', ', y);
          // DrawShape(Tiles[y, x].TileShape);
          
          if Tiles[y, x].Kind = k then
          begin
            if SpriteShapeCollision(s, Tiles[y,x].TileShape) then
            begin
              result := true;
              collidedX := x;
              collidedY := y;
              exit;
            end;
          end;
        end;
      end;
    end; // with
    
    collidedX := -1;
    collidedY := -1;
  end;

  procedure MoveOut(m: map; s: Sprite; x, y: Longint);
  var
    kickVector: Vector;
    sprRect: Rectangle;
    tileshape: shape;
    velocity: Vector;
  begin
    tileshape:= m^.Tiles[y,x].TileShape;
    sprRect := SpriteCollisionRectangle(s);
    velocity:= s^.Velocity;

    //Draw the sprite rectangle
    //DrawRectangle(ColorYellow, sprRect);
    //DrawShape(tileShape);
    //WriteLn(x, ',', y);
    kickVector := ShapeVectorOutOfRect(tileshape, sprRect, velocity);

    sprRect.x += kickVector.x;
    sprRect.y += kickVector.y;
    
    //DrawRectangle(ColorWhite, sprRect);
    //writeln('v.x: ',kickVector.x, 'v.y: ',kickVector.y);
    MoveSprite(s, kickVector);;
  end;




  //==================================================================
  //FUNCTIONS THAT RETURNS MAP PROPERTIES
  //==================================================================
  function BitmapCellKinds(m:map): BitmapCellKindArray;
  var
    i : Longint;
  begin
    SetLength(result,0);
    if NOT Assigned(m) then exit;
    SetLength(result, length(m^.BitmapCellKind));
    for i := low (m^.BitmapCellKind) to high (m^.BitmapCellKind) do
    begin
      result[i].Bmap := m^.BitmapCellKind[i].Bmap;
      result[i].Cell := m^.BitmapCellKind[i].Cell;
      result[i].KindIdx := m^.BitmapCellKind[i].KindIdx;
    end;
  end;

  
  function MapDefaultValues(m : map; const kId, vId : Longint): single;
  begin
    result := -1;
    if NOT Assigned(m) then exit;
  result := m^.MapDefaultValues[kId,vId];  
  end;
  
  function TileCount(m : map): Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit;
    result := (m^.MapWidth) * (m^.MapHeight);
  end;

  function IndexOfKind(const m :  map; const kname : string) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit;
    result := indexOf(m^.kindIds, kname);
  end;

  function IndexOfValues(const m :  map; const vName : string) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit;
    result := indexOf(m^.valueIds, vName);
  end;
  
  function MapKinds(m: map) : StringArray;
  var
    i : Longint;
  begin
    SetLength(result,0);
    result:= result;
    if not Assigned(m) then exit;
    SetLength(result,NameCount(m^.KindIds));
    for i := 0 to NameCount(m^.KindIds)-1 do
    begin
      result[i] := NameAt(m^.KindIds, i); 
    end;
  end;
  
  function MapValues(m: map) : StringArray;
  var
    i : Longint;
  begin
    SetLength(result,0);
    if not Assigned(m) then exit
    else
    SetLength(result,NameCount(m^.ValueIds));
    for i := 0 to NameCount(m^.ValueIds)-1 do
    begin
      result[i] := NameAt(m^.ValueIds, i); 
    end;
  end;


  function MapWidth(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapWidth;
  end;



  function Isometric(m: map) : Boolean;
  begin
    result:= false;
    if not Assigned(m) then exit
    else
    result := m^.Isometric;
  end;
  
  function MapHeight(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapHeight;
  end;

  function MapPrototype(m: Map) : ShapePrototype;
  begin
    result := m^.MapPrototype;
  end;

  function CountSelectedTiles(m: Map) : Longint;
  begin
    result := -1;
    if NOT assigned(m) then exit
    else
    result:= length(m^.SelectedTiles);
  end;

  function SelectedTiles(m: Map) : LongintArray;
  begin
    result := nil;
    if NOT Assigned(m) then exit
    else
    result := m^.Selectedtiles;
  end;

  function MapHighlightcolor(m: Map) : color;
  begin
    result := m^.MapHighlightcolor;
  end;

  
  function TileWidth(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.TileWidth;
  end;
  
  function TileHeight(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.TileHeight;
  end;

  function LayerCount(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapLayer;
  end;

  function TileStagger(m: Map) : Vector;
  begin
    result := VectorTo(0,0);
    if NOT Assigned(m) then exit
    else
    result := m^.TileStagger;
  end;


  //=========================//
  //TILE RETURN FUNCTIONS    //
  //=========================//


  function TileAt(m: Map; id:Longint) : Tile; Overload;
  var
    row,col : Longint;
  begin
    result := nil;
    row := id div m^.MapWidth;
    col := id mod m^.MapWidth;
    if (row >= m^.MapHeight) AND (col >= m^.MapWidth) then exit
    else
    result := TileAt(m, row,col);
  end;

  function TileAt(m: Map; row, col: Longint): Tile; overload;
  begin    
    if (not assigned(m)) or (row < Low(m^.Tiles)) or (row > High(m^.Tiles)) or (col < Low(m^.Tiles[row])) or (col > High(m^.Tiles[row])) then
      result := nil
    else
      result := @m^.Tiles[row,col];
  end;

  function TileAt(m: Map; const pos:Point2D): Tile; overload;
  var
    startrow,startcol : Longint;
    endrow,endcol : Longint;
    row,col : Longint;
  begin
    result:=nil;
    if (pos.y <0) or (pos.x <0) or (m^.TileWidth = 0) or (m^.TileHeight = 0) then exit;
    MapRangeFromRect(RectangleFrom(pos.x,pos.y,1,1),m, startCol, startRow, endCol, endRow);
    //writeln('startrow: ', startRow, 'startCol: ', startcol, 'endrow: ',endrow, 'endCol: ', endcol);
    for row := startrow to endrow do
    begin
      for col := startcol to endcol do
      begin
        if PointInShape(pos, m^.tiles[row,col].TileShape) then
        result := TileAt(m, row, col);
      end;
    end;
  end;

  function TileKind(t: Tile) : Longint;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.Kind;
  end;


  function TileValue(t: Tile; vId: Longint) : Single;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.Values[vId];
  end;



  function TilePosition(t: tile): Point2D;
  begin
    result.X := -1;
    result.Y := -1;
    if not Assigned(t) then exit
    else
    result := t^.Position;
  end;

  function TileCenter(t: tile): Point2D;
  begin
    result.X := -1;
    result.Y := -1;
    if not Assigned(t) then exit
    else
    result := t^.Center;
  end;


  function TileShape(t: tile): Shape;
  begin
    result := nil;
    if NOT assigned(t) then exit
    else
    result := t^.TileShape;
  end;

  function TileNeighbour(t: tile; d:Direction): Tile;
  begin
    result := nil;
    if NOT Assigned(t) then exit
    else
    result := t^.SurroundingTiles[d];
  end;

  function TileId(t: Tile): Longint;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.TileId;
  end;

  function TileBitmap(t: Tile; layer:Longint): Bitmap;
  begin
    result := nil;
    if not Assigned(t) then exit
    else
    result := t^.TileBitmapCellKind[layer]^.BMap;
  end;

  function KindName(m : map; idx:Longint): String;
  begin
    result := '';
    if not Assigned(m) then exit
    else
    result:= NameAt(m^.KindIds, idx);
  end;
  
  function ValueName(m : map; idx:Longint): String;
  begin
    result := '';
    if not Assigned(m) then exit
    else
    result:= NameAt(m^.ValueIds, idx);
  end;
  //======================//
  // Set Tile procedures  //
  //======================//


  
  procedure SetTileBitmap(m:map; t : tile; layer:Longint; idx :Longint);
  begin
    if not assigned(t) then exit;
    t^.TileBitmapCellKind[layer] := @m^.BitmapCellKind[idx]
  end;
  
  procedure SetTileKind(t: Tile; kindId:Longint);
  begin
    t^.Kind := KindId;
  end;


  procedure SetTileValue(t : Tile; vId : Longint; value : Single);
  begin
    if not Assigned(t) or (vId < 0) OR (vId > High(t^.values)) then exit;
    t^.Values[vId] := value;
   // writeln('the value is ',value);
  end;


  procedure SetTileValue(m:map; t :Tile; name : String; val : Single);
  var
    idx: Longint;
  begin  
    if not assigned(t) then exit;
    idx := IndexOf(m^.valueIds, name);
    //writeln('idx',idx);
    SetTileValue(t, idx, val);
  end;
  

  //======================//
  // Set map procedures   //
  //======================//

  procedure ReAssignKinds(m:map);
  var
    row,col : Longint;
  begin
    for row := low(m^.Tiles) to high(m^.Tiles) do
    begin
      for col := low(m^.Tiles[row]) to high(m^.Tiles[row]) do
      begin
        if (m^.Tiles[row,col].TileBitmapCellKind[0] = nil) then
        begin
          m^.Tiles[row,col].Kind := -1;
          break;
        end; 
        m^.Tiles[row,col].Kind := m^.Tiles[row,col].TileBitmapCellKind[0]^.KindIdx;
      end;
    end;
  end;
  

  procedure MapSetBitmapDefaultKind(m: map; const idx :longInt; kind :longInt);
  begin
  m^.BitmapCellKind[idx].KindIdx := kind;
  end;

  procedure MapRemoveBitmap(m : map; const idx:Longint);
  var
    i,k,row,col : Longint;
    NewBitmapCellKinds : array of BitmapCellKind;
    oldAdd, newAdd : Array of Pointer;
    
    procedure _MakeNewBitmapCellKind();
    var
      j : Longint;
    begin
      setLength(NewBitmapCellKinds, length(m^.BitmapCellKind)-1);
      setLength(oldAdd, length(m^.BitmapCellKind));
      setLength(newAdd, length(m^.BitmapCellKind));
      for j := low(m^.BitmapCellKind) to idx-1 do //before the to be removed index is reached
      begin
        oldAdd[j] := @m^.BitmapCellKind[j]; // stores the old address 
        NewBitmapCellKinds[j] := m^.BitmapCellKind[j];  // copy to new bitmapcellkind array
        newAdd[j] := @NewBitmapCellKinds[j]; // store the new map... note that the index of old and new are the same but addresses are different.
      end; // end of first loop
      oldAdd[idx] := @m^.BitmapCellKind[idx]; // copy the address of the tobe removed index to the old address
      newAdd[idx] := nil; // since the index is about to be removed the new address is nil.
      for j := idx+1 to high(m^.BitmapCellKind) do //loop after the removed index. note that j is the index of the old and j-1 is the index of the new
      begin
        oldAdd[j] := @m^.BitmapCellKind[j]; // store the old address of bitmapcell kind
        NewBitmapCellKinds[j-1] := m^.BitmapCellKind[j]; // new bitmapcellkind is now lagging by 1 due to the removed index.
        newAdd[j] := @NewBitmapCellKinds[j-1];   // address f new bitmapcellkind
      end;
    end;
  begin
    _MakeNewBitmapCellKind();
    for row := low(m^.Tiles) to high(m^.Tiles) do
    begin
      for col := low(m^.Tiles[row]) to high(m^.Tiles[row]) do
      begin
        for i :=low(m^.Tiles[row,col].TileBitmapCellKind) to high(m^.Tiles[row,col].TileBitmapCellKind) do // loop thru the arrays of bitmapcellkindptr
        begin
          for k := low(oldAdd) to high(oldAdd) do  // loops thru the arrays of pointers
          begin
            if (m^.Tiles[row,col].TileBitmapCellKind[i] = oldAdd[k]) then // if tilebitmapcell kind points to the old address then
            begin
            //  writeln(i,k);
              //writeln('current Address: ', hexstr(m^.Tiles[row,col].TileBitmapCellKind[i]), 'to ', hexstr(newAdd[k]));
              m^.Tiles[row,col].TileBitmapCellKind[i] := newAdd[k]; // assign the new address
            end; // if the tilebitmapcell doesnt match the old address it will just go to the next loop (should anyways :s)
          end;
        end; // end of TileBitmapCell Loop
      end; // end of col
    end;// end of row
     m^.BitmapCellKind := NewBitmapCellKinds;
  end; 
  
  procedure AddMapValues(m : map;const  idx1,idx2 : Longint;const  val : Single);
  begin
    if (idx1 = -1) or (idx2 = -1) then exit;
    m^.MapDefaultValues[idx1,idx2] := val;
    //writeln(val);
    ReconfigureMap(m);
  end;
  
  procedure AddKind(m:map; kname:string);
  var
    i,j: Longint;
  begin
    if length(kname) = 0 then exit;
    AddName(m^.kindIds, kname);
    SetLength(m^.MapDefaultValues, length(m^.MapDefaultValues)+1);
    for i := low(m^.MapDefaultValues) to high(m^.MapDefaultValues) do
    begin
      setlength(m^.MapDefaultValues[i], NameCount(m^.valueIds));
    end;
    for j := low(m^.MapDefaultValues[i]) to high(m^.MapDefaultValues[i]) do
    begin
      m^.MapDefaultValues[high(m^.MapDefaultValues),j] :=0;
    end;
  end;
  procedure MapAddValue(m:map; vName:string);
  var
    i:Longint;
  begin
    if length(vName) = 0 then exit;
    AddName(m^.valueIds, vName);
    for i := low (m^.MapDefaultValues) to high(m^.MapDefaultValues) do
    begin
      setlength(m^.MapDefaultValues[i], Length(m^.MapDefaultValues[i])+1);
      m^.MapDefaultValues[i,high(m^.MapDefaultValues[i])] := 0;
    end;
    ReconfigureMap(m);
  end;

// set values of kind removed to 0 then set kind to -1

  procedure RemoveKind(m:map; idx:Longint); overload;
  var
    row,col,i : Longint;
  begin
    if not assigned(m) then exit;
    RemoveName(m^.kindIds, NameAt(m^.kindids, idx));
    for row := Low(m^.Tiles) to High(m^.Tiles) do
    begin
      for col := Low(m^.Tiles[row]) to High(m^.Tiles[row]) do
      begin
        if m^.Tiles[row,col].Kind = idx then
        begin
          m^.Tiles[row,col].Kind := -1;
          for i := low(m^.Tiles[row,col].values) to high(m^.Tiles[row,col].values) do
          begin
            m^.Tiles[row,col].Values[i] := 0;
          end;//end of values
        end//end of if kind = idx
        else if m^.Tiles[row,col].Kind > idx then m^.Tiles[row,col].Kind -= 1;
      end;//endcol
    end;//end shuffling tile to have proper kinds (endrow)
    for i := low(m^.MapDefaultValues) to high(m^.MapDefaultValues) do
    begin
      if i = idx then
      begin
        m^.MapDefaultValues[i] := m^.MapDefaultValues[i+1];
      end;
    end;
    setLength(m^.MapDefaultValues, length(m^.MapDefaultValues)-1);
  end;

  procedure RemoveKind(m:map; kname:string); overload;
  begin
    RemoveKind(m, indexOf(m^.kindIds,kname));
  end;

  

  
  procedure RemoveValue(m:map; idx:Longint); overload;
  var
    row,col,vIdx,i : Longint;
    
  begin
    if not assigned(m)  then exit;
    RemoveName(m^.valueIds, NameAt(m^.valueIds, idx));
    for row := Low(m^.Tiles) to High(m^.Tiles) do
    begin
      for col := Low(m^.Tiles[row]) to High(m^.Tiles[row]) do
      begin
        for vIdx := low(m^.Tiles[row,col].values) to high(m^.Tiles[row,col].values) do
        begin
          if vIdx = idx then
          begin
            m^.Tiles[row,col].values[vIdx] := m^.Tiles[row,col].values[vIdx+1];
          end;
        end;//end of values
        setlength(m^.Tiles[row,col].Values, Length(m^.Tiles[row,col].values)-1);
      end;//end of col
    end;// end of row
    for i := low(m^.MapDefaultValues) to high(m^.MapDefaultValues) do
    begin
      SetLength(m^.MapDefaultValues[i], Length(m^.MapDefaultValues[i])-1);
    end;
  end;

  procedure RemoveValue(m:map; vName:string);overload;
  begin
    RemoveValue(m, IndexOf(m^.valueIds,vName));
  end;

   // PRIVATE FUNCTION
    procedure RewireTilesToBitmapCellKind(m:map; oldBCKArray,newBCKArray: array of pointer );
    var
      row,col,layer,i : Longint;
    begin
      for row := low(m^.Tiles) to high(m^.Tiles) do // loop row
      begin 
        for col := low(m^.Tiles[row]) to high(m^.Tiles[row]) do // loop col
        begin
          for layer := low(m^.Tiles[row,col].TileBitmapCellKind)  to high(m^.Tiles[row,col].TileBitmapCellKind) do // loop layer
          begin
            for i := low(oldBCKArray) to high(oldBCKArray) do // loop the old addresses
            begin
              
              if m^.Tiles[row,col].TileBitmapCellKind[layer] = oldBCKArray[i] then // if tile's bckarray points to old address then
              begin
                //writeln('old: ',hexstr(oldBCKArray[0]), ' new: ',hexstr(@newBCKArray[0]));
                m^.Tiles[row,col].TileBitmapCellKind[layer] := newBCKArray[i]; // assigne new address.
                break;// breaks to layer
                
              end;
            end; // end old address loop
          end; // end layer loop
        end;// end col loop
      end;// end row loop
    end; // end of procedure.
  
  procedure MapSetDimension(m : map;  Width, height, layers, tWidth, tHeight : Longint; iso:Boolean);
  begin
    if not assigned(m) or (Width < 0) or  (height < 0) or  (Layers < 0) or  (tWidth < 0) or  (tHeight < 0) then exit;
    m^.MapHeight := height;
    m^.MapWidth  := width;
    m^.MapLayer  := layers;
    m^.TileWidth := tWidth;
    m^.TileHeight:= tHeight;
    m^.Isometric := iso;
    ReconfigureMap(m);
  end;


    procedure MapAddBitmap(m:map; filename:String);
    var
      index,i:Longint;
      oldBCKArray, newBCKArray : Array Of Pointer;
    begin
      SetLength(oldBCKArray, length(m^.BitmapCellKind));
      for i := Low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
      begin
        oldBCKArray[i] := @m^.BitmapCellKind[i];
      end;
      index:=MyStrToInt(ExtractDelimited(1,filename,[',']),false);
      if (index+1)> length(m^.BitmapCellKind) then
      begin
        SetLength(m^.BitmapCellKind, index+1);
        m^.BitmapCellKind[index].Bmap := LoadBitmap(ExtractDelimited(2,filename,[',']));
        m^.BitmapCellKind[index].Cell:= 0;
      end;
      
      SetLength(newBCKArray, length(m^.BitmapCellKind));
      for i := Low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
      begin
        newBCKArray[i] := @m^.BitmapCellKind[i];
      end;
      
      RewireTilesToBitmapCellKind(m, oldBCKArray, newBCKArray);
    end;


    procedure MapAddBitmapCells(m : map; bitmapCellIds : array of Longint; cellRegions : array of Longint; gridBitmap : Bitmap);
    var
    i ,j  : Longint;
    oldBCKArray, newBCKArray : Array of pointer;
    begin
      SetLength(oldBCKArray, length(m^.BitmapCellKind));
        for j := Low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
        begin
          oldBCKArray[j] := @m^.BitmapCellKind[j];
        end;
        
      for i:=low(bitmapCellIds) to high(bitmapCellIds) do
      begin

        SetLength(m^.BitmapCellKind, length(m^.BitmapCellKind)+1);
        m^.BitmapCellKind[high(m^.BitmapCellKind)].Cell := cellRegions[i];
        m^.BitmapCellKind[high(m^.BitmapCellKind)].Bmap  := gridBitmap;
      end;
      SetLength(newBCKArray, length(m^.BitmapCellKind));
      for j := Low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
      begin
        newBCKArray[j] := @m^.BitmapCellKind[j];
      end;
      RewireTilesToBitmapCellKind(m, oldBCKArray, newBCKArray);
    end;

  function NewMap():map;
  begin
    new(result);
    with result^ do
    begin
      SetLength(SelectedTiles, 0);
      InitNamedIndexCollection(result^.valueIds);
      InitNamedIndexCollection(result^.kindids);
      SetLength(MapDefaultValues ,0,0);
      SetLength(BitmapCellKind,0);
      SetLength(Tiles, 0);
      MapWidth          := 0;
      MapHeight         := 0;
      Isometric         := false;        
      MapHighlightcolor := RGBAColor(0,255,0,255);
      MapLayer          := 0;        
      TileWidth         := 0; 
      TileHeight        := 0;      
      TileStagger       := VectorTo(0,0);    
      MapPrototype      := nil;
    end;
  end;

  procedure ReconfigureMap(var m: map);
  var
    row,col,id : Longint;
    
    procedure _CreateSurroundingTiles(tile : Tile; row, col : Longint);
    begin
      if NOT (m^.Isometric) then
      begin
        tile^.SurroundingTiles[mdNorthWest]  := TileAt(m, row-1,  col-1);
        tile^.SurroundingTiles[mdNorth]      := TileAt(m, row-1,  col);
        tile^.SurroundingTiles[mdNorthEast]  := TileAt(m, row-1,  col+1);
        tile^.SurroundingTiles[mdWest]       := TileAt(m, row,    col-1);
        tile^.SurroundingTiles[mdEast]       := TileAt(m, row,    col+1);
        tile^.SurroundingTiles[mdSouthWest]  := TileAt(m, row+1,  col-1);
        //writeln(tile.SurroundingTiles[mdSouthWest]^.TileId);
        tile^.SurroundingTiles[mdSouth]      := TileAt(m, row+1,  col);
        tile^.SurroundingTiles[mdSouthEast]  := TileAt(m, row+1,  col+1);
      end
      else
      begin
        tile^.SurroundingTiles[mdNorthWest]  := TileAt(m, row-1,  col);
        tile^.SurroundingTiles[mdNorth]      := TileAt(m, row-2,  col);
        tile^.SurroundingTiles[mdNorthEast]  := TileAt(m, row-1,  col+1 -((row mod 2)*2));
        tile^.SurroundingTiles[mdWest]       := TileAt(m, row,    col-1);
        tile^.SurroundingTiles[mdEast]       := TileAt(m, row,    col+1);
        tile^.SurroundingTiles[mdSouthWest]  := TileAt(m, row+1,  col);
        tile^.SurroundingTiles[mdSouth]      := TileAt(m, row+2,  col);
        tile^.SurroundingTiles[mdSouthEast]  := TileAt(m, row+1,  col+1 -((row mod 2)*2));
      end;
    end;

    procedure _AddMapPrototype();
    var
      pts:Point2DArray;
    begin
      SetLength(Pts, 5);
      pts[0].X:=0;
      pts[0].Y:=(m^.TileStagger.Y);
      pts[1].X:=((-m^.TileStagger.X)+m^.TileWidth);
      pts[1].Y:=0;
      pts[2].X:=m^.TileWidth;
      pts[2].Y:=(m^.TileHeight - m^.TileStagger.Y);
      pts[3].X:=(m^.TileStagger.X);
      pts[3].Y:=(m^.TileHeight);
      pts[4].X:=0;
      pts[4].Y:=(m^.TileStagger.Y);
      m^.MapPrototype:=PrototypeFrom(pts, pkTriangleStrip);
    end;    
  begin
    id:=0; // initiate the ids.
    SetLength(m^.Tiles, m^.MapHeight, m^.MapWidth); //set lengths of tiles

    // Set tile stagger information
    if m^.Isometric then
    begin
       m^.TileStagger.X := (m^.TileWidth/2);
       m^.TileStagger.Y := (m^.TileHeight/2);
    end
    else
    begin
      m^.TileStagger.X := 0;
      m^.TileStagger.Y := 0;
    end;

    _AddMapPrototype();
    for row:=low(m^.Tiles) to high (m^.Tiles) do
    begin
      for col:=low(m^.Tiles[row]) to high(m^.Tiles[row]) do
      begin
        SetLength(m^.Tiles[row,col].TileBitmapCellKind, m^.MapLayer); // set length of bitmapcells per tile.
        if (m^.Tiles[row,col].TileBitmapCellKind[0] = nil) then
        begin
          m^.Tiles[row,col].Kind := -1;
        end
        else
        begin
          m^.Tiles[row,col].Kind := m^.Tiles[row,col].TileBitmapCellKind[0]^.KindIdx;
        end;
        m^.Tiles[row,col].TileID:=id;
        id+=1;
        //Allocate position and center given stagger
        m^.Tiles[row,col].Position.X  := (col * m^.TileWidth) - ((row mod 2) * m^.TileStagger.X);      // stagger alternate lines
        m^.Tiles[row,col].Position.Y  := (row * m^.TileHeight) - ((row + 1) * m^.TileStagger.Y);       // position y value
        m^.Tiles[row,col].Center.X    := m^.Tiles[row,col].Position.X + (m^.TileWidth/2);                
        m^.Tiles[row,col].Center.Y    := m^.Tiles[row,col].Position.Y + (m^.TileHeight/2);
        m^.Tiles[row,col].TileShape   := ShapeAtPoint(m^.MapPrototype, m^.Tiles[row,col].Position);   // Shape of the tile
        m^.Tiles[row,col].TileShape   := ShapeAtPoint(m^.MapPrototype, m^.Tiles[row,col].Position);   // Shape of the tile
        
        ShapeSetColor(m^.Tiles[row,col].TileShape, m^.mapHighlightcolor);
        _CreateSurroundingTiles(@m^.Tiles[row,col],row,col);
        AllocateDefaultValues(m, m^.Tiles[row,col]);
      end;
    end;
  end;


//----------------------------------------------------------
//  Resource management
//----------------------------------------------------------

  function LoadMap(filename: String): Map;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMaps', 'LoadMap', filename);
    {$ENDIF}
    
    result := LoadMapNamed(filename, filename);
    
    {$IFDEF TRACE}
      TraceExit('sgMaps', 'LoadMap');
    {$ENDIF}
  end;


  function LoadMapNamed(name, filename: String): Map;
  var
    obj: tResourceContainer;
    mp : Map;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMap', 'MapMap', name + ' = ' + filename);
    {$ENDIF}
    
    
    if _Maps.containsKey(name) then
    begin
      result := MapNamed(name);
      exit;
    end;
    
    mp := DoLoadMap(filename, name);
    if not assigned(mp) then
    begin
        result := nil;
        exit;
    end;
    
    obj := tResourceContainer.Create(mp);
    
    if not _Maps.setValue(name, obj) then
    begin
      RaiseException('** Leaking: Caused by Sound m resource twice, ' + name);
      result := nil;
      exit;
    end;
    result := mp;
    
    {$IFDEF TRACE}
      TraceExit('sgMap', 'MapMap');
    {$ENDIF}
  end;

  procedure DoFreeMap(var m: Map);
  var
  i: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMap', 'DoFreeMap', 'map = ' + HexStr(m));
    {$ENDIF}
    
    if assigned(m) then
    begin
      CallFreeNotifier(m);
      for i:=0 to TileCount(m)-1 do
      begin
        FreeShape(TileAt(m,i)^.TileShape);
        //setlength(TileAt(m,i)^.Values, 0);
      end;
      SetLength(m^.Tiles, 0,0);
      SetLength(m^.MapDefaultValues, 0,0);
      FreeNamedIndexCollection(m^.ValueIds);
      FreeNamedIndexCollection(m^.KindIds);
      FreePrototype(m^.MapPrototype);
      Dispose(m);
    end;
    m := nil;
    {$IFDEF TRACE}
      TraceExit('sgMap', 'DoFreeMap');
    {$ENDIF}
  end;

  procedure FreeMap(var m: map);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMap', 'FreeMap', 'map = ' + HexStr(m));
    {$ENDIF}
    
    if(assigned(m)) then
    begin
      ReleaseMap(m^.name);
    end;
    m := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgMap', 'FreeMap');
    {$ENDIF}
  end;
  
  procedure ReleaseMap(name: String);
  var
    m: map;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMap', 'ReleaseMap', 'map = ' + name);
    {$ENDIF}
    
    m := MapNamed(name);
    if (assigned(m)) then
    begin
      _Maps.remove(name).Free();
      DoFreeMap(m);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgMap', 'ReleaseMap');
    {$ENDIF}
  end;
  
  procedure ReleaseAllMaps();
  begin
    {$IFDEF TRACE}
      TraceEnter('sgMap', 'ReleaseAllMaps', '');
    {$ENDIF}
    
    ReleaseAll(_Maps, @ReleaseMap);
    
    {$IFDEF TRACE}
      TraceExit('sgMap', 'ReleaseAllMaps');
    {$ENDIF}
  end;

    function HasMap(name: String): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAudio', 'HasMap', name);
    {$ENDIF}
    
    result := _Maps.containsKey(name);
    
    {$IFDEF TRACE}
      TraceExit('sgAudio', 'HasMap', BoolToStr(result, true));
    {$ENDIF}
  end;

  function MapNamed(name: String): Map;
  var
    tmp : TObject;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAudio', 'MapNamed', name);
    {$ENDIF}
    
    tmp := _Maps.values[name];
    if assigned(tmp) then result := Map(tResourceContainer(tmp).Resource)
    else result := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgAudio', 'MapNamed', HexStr(result));
    {$ENDIF}
  end;
  
  function MapName(m: Map): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAudio', 'MapName', HexStr(m));
    {$ENDIF}
    
    if assigned(m) then result := m^.name
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgAudio', 'MapName', result);
    {$ENDIF}
  end;
  
  function MapFilename(m: Map): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAudio', 'MapFilename', HexStr(m));
    {$ENDIF}
    
    if assigned(m) then result := m^.filename
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgAudio', 'MapFilename', result);
    {$ENDIF}
  end;
  


  
  initialization
  begin
    InitialiseSwinGame();
    _Maps := TStringHash.Create(False, 1024);
  end;
  
  finalization
  begin
    ReleaseAllMaps();
    FreeAndNil(_Maps);
  end;
  
end.



