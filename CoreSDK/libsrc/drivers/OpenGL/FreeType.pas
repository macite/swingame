unit FreeType;

interface

type
    FT_Int      = longint;
    FT_Int16    = smallint;
    FT_UInt16   = word;
    FT_Int32    = longint;
    FT_UInt32   = dword;
    FT_Fast     = longint;
    FT_UFast    = dword;
    FT_INT64    = int64;
    FT_Bytes    = ^FT_Byte;
    size_t      = PtrInt;

    FT_Bool     = byte;
    FT_FWord    = smallint;
    FT_UFWord   = word;
    FT_Char     = char;
    FT_Byte     = byte;
    FT_Tag      = FT_UInt32;
    FT_String   = char;
    FT_Short    = smallint;
    FT_UShort   = word;
    FT_UInt     = dword;
    FT_ULong    = dword;
    FT_F2Dot14  = smallint;
    FT_F26Dot6  = longint;
    
    {$IFDEF CPU64}
      FT_Fixed  = int64;
      FT_Long   = int64;
      FT_Pos     = Int64;
    {$ELSE}
      FT_Fixed  = longint;
      FT_Long   = longint;
      FT_Pos     = longint;
    {$ENDIF}
    
    FT_Error    = longint;
    FT_Pointer  = pointer;
    FT_Offset   = size_t;
    //FT_PtrDist  = ft_ptrdiff_t;
    
    FT_UnitVector = record
      x : FT_F2Dot14;
      y : FT_F2Dot14;
    end;

    FT_Matrix = record
      xx : FT_Fixed;
      xy : FT_Fixed;
      yx : FT_Fixed;
      yy : FT_Fixed;
    end;

    FT_Data = record
      pointer : ^FT_Byte;
      length : FT_Int;
    end;

    FT_Generic_Finalizer = procedure (aobject: pointer);cdecl;

    FT_Generic = record
      data      : pointer;
      finalizer : FT_Generic_Finalizer;
    end;

    // function FT_MAKE_TAG(_x1,_x2,_x3,_x4 : longint) : FT_Tag; 

    FT_ListNode = ^FT_ListNodeRec;
    FT_List = ^FT_ListRec;

    FT_ListNodeRec = record
      prev : FT_ListNode;
      next : FT_ListNode;
      data : pointer;
    end;

    FT_ListRec = record
      head : FT_ListNode;
      tail : FT_ListNode;
    end;

    // function FT_IS_EMPTY(list : longint) : longint;




	// FT_Int32	= LongInt;
    
    
    
    
 //    FT_Error 	= LongInt;
 //    FT_UInt 	= LongWord;
 //    FT_F26Dot6 	= LongWord;
 //    FT_ULong	= LongWord;
 //    FT_Pointer	= Pointer;
 //    FT_Bool		= Byte;
	// FT_Int 		= LongInt; //check
	// PByte 		= ^Byte;
	// FT_UShort 	= Word;
	// FT_Short 	= SmallInt;

	FT_Render_Mode = (
    	FT_RENDER_MODE_NORMAL = 0,
    	FT_RENDER_MODE_LIGHT,
    	FT_RENDER_MODE_MONO,
    	FT_RENDER_MODE_LCD,
    	FT_RENDER_MODE_LCD_V,

    	FT_RENDER_MODE_MAX
    	);

    FT_Kerning_Mode = (
        FT_KERNING_DEFAULT  = 0,
        FT_KERNING_UNFITTED,
        FT_KERNING_UNSCALED
        );

	FT_Library = 	Pointer;
    FT_Face = 		^FT_FaceRec;
    FT_Glyph = 		^FT_GlyphRec;
    FT_GlyphSlot = 	^FT_GlyphSlotRec;
    FT_Size = 		Pointer;
    FT_SubGlyph =   Pointer;
    FT_Slot_Internal = Pointer;

	FT_Vector = record
	    x: FT_Pos;
	    y: FT_Pos;
	end;
	PFT_Vector = ^FT_Vector;

	FT_BBox = record
	    xMin, yMin: FT_Pos;
	    xMax, yMax: FT_Pos;
    end;

	FT_Bitmap = record
    	rows: 			LongInt;
    	width: 			LongInt;
    	pitch: 			LongInt;
    	buffer: 		PByte;
    	num_grays: 		SmallInt; //short
    	pixel_mode: 	Char;
    	palette_mode: 	Char;
    	palette: 		Pointer;
  	end;
  	PFT_Bitmap = ^FT_Bitmap;

	FT_GlyphRec = record
	    alibrary: 	FT_Library;
	    clazz:		Pointer; //PFT_Glyph_Class;
	    format:		LongInt; //FT_Glyph_Format;
	    advance:	FT_Vector;
	end;

    FT_BitmapGlyphRec = record
	    root: 	FT_GlyphRec;
	    left: 	FT_Int;
	    top: 	FT_Int;
	    bitmap: FT_Bitmap;
	end;

	// FT_Generic = record
	// 	data: Pointer;
 //      	finalizer: Pointer; //FT_Generic_Finalizer;
	// end;

	FT_CharMap = Pointer;

	// FT_ListRec = record
 //    	head, tail: Pointer;
 //    end;

	FT_FaceRec = record
    	num_faces			: FT_Long;
    	face_index			: FT_Long;

    	face_flags			: FT_Long;
    	style_flags			: FT_Long;

    	num_glyphs			: FT_Long;

    	family_name			: Pointer; //FT_String*;
    	style_name			: Pointer; //FT_String*;

    	num_fixed_sizes		: FT_Int;
    	available_sizes		: Pointer; //FT_Bitmap_Size* ;

    	num_charmaps		: FT_Int;
    	charmaps			: Pointer; //FT_CharMap* ;

    	generic				:FT_Generic;

	    //*# The following member variables (down to `underline_thickness') */
	    //*# are only relevant to scalable outlines; cf. @FT_Bitmap_Size    */
	    //*# for bitmap fonts.                                              */
    	bbox :				FT_BBox;

    	units_per_EM :		FT_UShort;
    	ascender :			FT_Short;
    	descender :			FT_Short;
    	height :			FT_Short;

    	max_advance_width:	FT_Short;
    	max_advance_height:	FT_Short;

    	underline_position:	FT_Short;
    	underline_thickness:FT_Short;

    	glyph:				FT_GlyphSlot;
    	size:				FT_Size;
    	charmap:			FT_CharMap;

    	//*@private begin */

    	driver:				Pointer; //FT_Driver;
    	memory:				Pointer; //FT_Memory;
    	stream:				Pointer; //FT_Stream;

    	sizes_list:			FT_ListRec;

    	autohint:			FT_Generic;
    	extensions:			Pointer;

    	internal:			Pointer; //FT_Face_Internal;
    	//*@private end */
  end;

  FT_Glyph_Metrics = record
    width           : FT_Pos;
    height          : FT_Pos;
 
    horiBearingX    : FT_Pos;
    horiBearingY    : FT_Pos;
    horiAdvance     : FT_Pos;
 
    vertBearingX    : FT_Pos;
    vertBearingY    : FT_Pos;
    vertAdvance     : FT_Pos;
  end;

  FT_Outline = record
    n_contours: word; // number of contours in glyph        */
    n_points:   word;   // number of points in the glyph      */

    points:     PFT_Vector;     // the outline's points
    tags:       PChar;       // the points flags                   */
    contours:   ^word;   // the contour end points             */

    flags:      LongInt;      // outline masks                      */

  end;

  FT_GlyphSlotRec = record
    alibrary:           FT_Library;
    face:               FT_Face;
    next:               FT_GlyphSlot;
    reserved:           FT_UInt;       // retained for binary compatibility
    generic:            FT_Generic;

    metrics:            FT_Glyph_Metrics;
    linearHoriAdvance:  FT_Fixed;
    linearVertAdvance:  FT_Fixed;
    advance:            FT_Vector;

    format:             longint; //FT_Glyph_Format;

    bitmap:             FT_Bitmap;
    bitmap_left:        FT_Int;
    bitmap_top:         FT_Int;

    outline:            FT_Outline;

    num_subglyphs:      FT_UInt;
    subglyphs:          FT_SubGlyph;

    control_data:       Pointer;
    control_len:        longint;

    lsb_delta:          FT_Pos;
    rsb_delta:          FT_Pos;

    other:              Pointer;

    internal:           FT_Slot_Internal;
  end;
	// record  FT_Glyph_Class
	//    glyph_size:  	FT_Long;
	//    glyph_format:  	FT_Glyph_Format;
	//    glyph_init:  	FT_Glyph_InitFunc;
	//    glyph_done:  	FT_Glyph_DoneFunc;
	//    glyph_copy:  	FT_Glyph_CopyFunc;
	//    glyph_transform: FT_Glyph_TransformFunc;
	//    glyph_bbox:  	FT_Glyph_GetBBoxFunc;
	//    glyph_prepare:  	FT_Glyph_PrepareFunc;
	// end;


    PFT_Library = ^FT_Library;
    PFT_Face = ^FT_Face;
    PFT_Glyph = ^FT_Glyph;
    
    FT_BitmapGlyph = ^FT_BitmapGlyphRec;
    PFT_BitmapGlyph = ^FT_BitmapGlyph;
    

const FT_TRUE	= 1;
const FT_FALSE	= 0;

const FT_LOAD_DEFAULT                      = $0;
const FT_LOAD_NO_SCALE                     = $1;
const FT_LOAD_NO_HINTING                   = $2;
const FT_LOAD_RENDER                       = $4;
const FT_LOAD_NO_BITMAP                    = $8;
const FT_LOAD_VERTICAL_LAYOUT              = $10;
const FT_LOAD_FORCE_AUTOHINT               = $20;
const FT_LOAD_CROP_BITMAP                  = $40;
const FT_LOAD_PEDANTIC                     = $80;
const FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH  = $200;
const FT_LOAD_NO_RECURSE                   = $400;
const FT_LOAD_IGNORE_TRANSFORM             = $800;
const FT_LOAD_MONOCHROME                   = $1000;
const FT_LOAD_LINEAR_DESIGN                = $2000;
const FT_LOAD_NO_AUTOHINT                  = $8000;


function FT_Init_FreeType( alibrary: PFT_Library ): FT_Error; cdecl; external;
function FT_Done_FreeType( alibrary: PFT_Library ): FT_Error; cdecl; external;

function FT_New_Face( alibrary: FT_Library; filepathname: PChar; face_index: FT_Long; aface: PFT_Face ): FT_Error; cdecl; external;
function FT_Done_Face(face: FT_Face): FT_Error; cdecl; external;

function FT_Set_Char_Size( 	face: FT_Face;
                    		char_width: FT_F26Dot6;
                    		char_height: FT_F26Dot6;
                         	horz_resolution: FT_UInt;
                    		vert_resolution: FT_UInt ): FT_Error; cdecl; external;

function FT_Load_Glyph( face: 			FT_Face;
                 		glyph_index: 	FT_UInt;
                 		load_flags:		FT_Int32 ): FT_Error; cdecl; external;

function FT_Get_Char_Index( face: 		FT_Face;
                     		charcode: 	FT_ULong ): FT_Error; cdecl; external;

function FT_Get_Glyph_Name(	face:  			FT_Face;
                     		glyph_index:	FT_UInt;
                     		buffer:			FT_Pointer;
                          	buffer_max:		FT_UInt ): FT_Error; cdecl; external;

function FT_Get_Glyph( 	slot: 		FT_GlyphSlot;
  	                  	aglyph: 	PFT_Glyph ): FT_Error; cdecl; external;

function FT_Glyph_To_Bitmap( 	the_glyph: 		PFT_Glyph;
                      			render_mode: 	FT_Render_Mode;
                      			origin: 		PFT_Vector;
                               	destroy:		FT_Bool ): FT_Error; cdecl; external;

function FT_Get_Kerning(    face:           FT_Face;
                            left_glyph:     FT_UInt;
                            right_glyph:    FT_UInt;
                            kern_mode:      FT_Kerning_Mode;
                            akerning:       PFT_Vector) : FT_Error; cdecl; external;

    const 
        FT_FACE_FLAG_SCALABLE         = 1 shl  0;
        FT_FACE_FLAG_FIXED_SIZES      = 1 shl  1;
        FT_FACE_FLAG_FIXED_WIDTH      = 1 shl  2;
        FT_FACE_FLAG_SFNT             = 1 shl  3;
        FT_FACE_FLAG_HORIZONTAL       = 1 shl  4;
        FT_FACE_FLAG_VERTICAL         = 1 shl  5;
        FT_FACE_FLAG_KERNING          = 1 shl  6;
        FT_FACE_FLAG_FAST_GLYPHS      = 1 shl  7;
        FT_FACE_FLAG_MULTIPLE_MASTERS = 1 shl  8;
        FT_FACE_FLAG_GLYPH_NAMES      = 1 shl  9;
        FT_FACE_FLAG_EXTERNAL_STREAM  = 1 shl 10;
        FT_FACE_FLAG_HINTER           = 1 shl 11;
        FT_FACE_FLAG_CID_KEYED        = 1 shl 12;
        FT_FACE_FLAG_TRICKY           = 1 shl 13;
    
    function FT_HAS_KERNING( face : FT_Face ) : Boolean;

implementation

function FT_HAS_KERNING( face : FT_Face ) : Boolean;
begin
    if assigned(face) then result := (face^.face_flags and FT_FACE_FLAG_KERNING) = FT_FACE_FLAG_KERNING
    else result := false;
end;
          
end.

