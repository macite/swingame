unit sgDriverSDL2Types;

interface

	//
	// Types from sgBackendTypes.pas
	//
  type
    int = longint;
    pint = ^int;
    float = single;
    pfloat = ^single;
    uint = longword;

    sg_drawing_surface_kind = (
    	SGDS_Unknown := 0,
    	SGDS_Window  := 1,
      SGDS_Bitmap  := 2 );


    sg_drawing_surface = packed record
        kind : sg_drawing_surface_kind;
        width : longint;
        height : longint;
        _data : pointer;
      end;
    sg_renderer_flip = (SG_FLIP_NONE := 0,SG_FLIP_HORIZONTAL := 1,
      SG_FLIP_VERTICAL := 2,SG_FLIP_BOTH := 3
      );


    sg_mode = packed record
        format : uint;
        width : longint;
        height : longint;
        refresh_rate : longint;
      end;

    sg_display = packed record
        name : pchar;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        refresh_rate : longint;
        format: dword;
        num_modes : dword;
        modes : ^sg_mode;
        _data : pointer;
      end;

    sg_audiospec = packed record
        audio_rate : longint;
        audio_format : longint;
        audio_channels : longint;
        times_opened : longint;
      end;

    sg_system_data = packed record
        num_displays : dword;
        displays : ^sg_display;
        audio_specs : sg_audiospec;
      end;
    sg_font_kind = (SGFT_UNKNOWN := 0,SGFT_TTF := 1);


    sg_font_data = packed record
        kind : sg_font_kind;
        _data : pointer;
      end;
    sg_sound_kind = (SGSD_UNKNOWN := 0,SGSD_SOUND_EFFECT := 1,
      SGSD_MUSIC := 2);


    sg_sound_data = packed record
        kind : sg_sound_kind;
        _data : pointer;
      end;

    sg_font_style = (SG_FONT_STYLE_NORMAL := 0,SG_FONT_STYLE_BOLD := 1,
      SG_FONT_STYLE_ITALIC := 2,SG_FONT_STYLE_UNDERLINE := 4,
      SG_FONT_STYLE_STRIKETHROUGH := 8);


  //
  // sgInterface types
  //
  type
    sg_color = packed record
        r : single;
        g : single;
        b : single;
        a : single;
      end;

    psg_system_data = ^sg_system_data;
    psg_drawing_surface = ^sg_drawing_surface;
    psg_font_data = ^sg_font_data;
    psg_sound_data = ^sg_sound_data;
    psg_interface = ^sg_interface;

    //
    // Function pointers
    //
    sg_empty_procedure = procedure(); cdecl;
    sg_charp_fn = function(): pchar; cdecl;
    sg_charp_proc = procedure(ttext: pchar); cdecl;
    sg_rectangle_dimensions_proc = procedure(x, y, w, h: int); cdecl;
    sg_drawing_surface_proc = procedure(surface: psg_drawing_surface); cdecl;
    sg_drawing_surface_bool_fn = function(surface: psg_drawing_surface): int; cdecl;
    sg_single_uint32param_proc = procedure(ms: uint); cdecl;
    sg_drawing_surface_clr_proc = procedure(surface: psg_drawing_surface; clr: sg_color); cdecl;
    sg_drawing_proc = procedure(surface: psg_drawing_surface; clr: sg_color;data: pfloat; data_sz: int); cdecl;
    sg_clip_proc = procedure(surface: psg_drawing_surface; data: pfloat; data_sz: int); cdecl;
    sg_surface_bool_proc = procedure(surface: psg_drawing_surface; val: int); cdecl;
    sg_to_pixel_array_proc = procedure(surface: psg_drawing_surface; pixels: pint; sz: int); cdecl;

    sg_surface_size_proc = procedure(surface: psg_drawing_surface; width, height: int); cdecl;

    sg_new_surface_fn = function(title: pchar; width, height: int): sg_drawing_surface; cdecl;
    sg_create_surface_fn = function(width, height: int): sg_drawing_surface; cdecl;

    sg_surface_color_fn = function(surface: psg_drawing_surface; x, y: int): sg_color; cdecl;

    sg_system_data_fn = function(): psg_system_data; cdecl;

    //
    // Text-related function pointers
    //
    sg_font_load_fn = function(filename: pchar; font_size: int): sg_font_data; cdecl;
    sg_font_data_proc = procedure(font: psg_font_data); cdecl;
    sg_font_int_fn = function(font: psg_font_data): int; cdecl;
    sg_font_int_proc = procedure(font: psg_font_data; style: int); cdecl;
    sg_font_size_fn = function(font: psg_font_data; ttext: pchar; w, h: pint): int; cdecl;
    sg_draw_text_proc = procedure(surface: psg_drawing_surface; font: psg_font_data; x, y: float; ttext: pchar; clr: sg_color); cdecl;

    //
    // Sound-related function pointers
    //
    sg_sound_load_fn = function(filename: pchar; kind: sg_sound_kind): sg_sound_data; cdecl;
    sg_play_sound_fn = procedure(sound: psg_sound_data; loops: int; volume: float); cdecl;
    sg_sound_data_proc = procedure(sound: psg_sound_data); cdecl;
    sg_sound_float_fn = function(sound: psg_sound_data): float; cdecl;
    sg_sound_fade_proc = procedure(sound: psg_sound_data; loops, ms: int); cdecl;
    sg_sound_fade_out_proc = procedure(sound: psg_sound_data; ms: int); cdecl;
    sg_sound_float_proc = procedure(sound: psg_sound_data; val: float); cdecl;
    sg_intp_proc = procedure(ms: int); cdecl;
    sg_floatp_proc = procedure(val: float); cdecl;
    sg_float_fn = function(): float; cdecl;
    sg_int_intp_fn = function(val: int): int; cdecl;
    sg_float_soundp_fn = function(sound: psg_sound_data): float; cdecl;

    //
    // Utility related
    //
    uint_fn = function(): uint; cdecl;

    // 
    // Image related
    //
    sg_load_surface_fn = function(title: pchar): sg_drawing_surface; cdecl;
    sg_drawing_surface_surface_proc = procedure(src: psg_drawing_surface; dst: psg_drawing_surface; x, y: float; angle: double; centre_x, centre_y: float; scale: double; flip: sg_renderer_flip); cdecl;

    //
    // Input related
    //
    sg_mouse_state_fn = function(x, y: pint): uint; cdecl;
    sg_surface_xy_proc = procedure(surface: psg_drawing_surface; x, y: int); cdecl;

    
    sg_utils_interface = packed record
        delay : sg_single_uint32param_proc;
        get_ticks : uint_fn;
      end;

    sg_image_interface = packed record
        create_bitmap : sg_create_surface_fn;
        load_bitmap : sg_load_surface_fn;
        draw_bitmap : sg_drawing_surface_surface_proc;
      end;

    sg_audio_interface = packed record
        open_audio : sg_empty_procedure;
        close_audio : sg_empty_procedure;
        load_sound_data : sg_sound_load_fn;
        close_sound_data : sg_sound_data_proc;
        play_sound : sg_play_sound_fn;
        sound_playing : sg_sound_float_fn;
        fade_in : sg_sound_fade_proc;
        fade_out : sg_sound_fade_out_proc;
        fade_music_out : sg_intp_proc;
        fade_all_sound_effects_out : sg_intp_proc;
        set_music_vol : sg_floatp_proc;
        music_vol : sg_float_fn;
        sound_volume : sg_float_soundp_fn;
        set_sound_volume : sg_sound_float_proc;
        pause_music : sg_empty_procedure;
        resume_music : sg_empty_procedure;
        stop_music : sg_empty_procedure;
        stop_sound : sg_sound_data_proc;
      end;

    sg_graphics_interface = packed record
        open_window : sg_new_surface_fn;
        close_drawing_surface : sg_drawing_surface_proc;
        refresh_window : sg_drawing_surface_proc;
        clear_drawing_surface : sg_drawing_surface_clr_proc;
        draw_aabb_rect : sg_drawing_proc;
        fill_aabb_rect : sg_drawing_proc;
        draw_rect : sg_drawing_proc;
        fill_rect : sg_drawing_proc;
        draw_triangle : sg_drawing_proc;
        fill_triangle : sg_drawing_proc;
        draw_circle : sg_drawing_proc;
        fill_circle : sg_drawing_proc;
        draw_ellipse : sg_drawing_proc;
        fill_ellipse : sg_drawing_proc;
        draw_pixel : sg_drawing_proc;
        draw_line : sg_drawing_proc;
        read_pixel : sg_surface_color_fn;
        set_clip_rect : sg_clip_proc;
        clear_clip_rect : sg_drawing_surface_proc;
        to_pixels : sg_to_pixel_array_proc;
        show_border : sg_surface_bool_proc;
        show_fullscreen : sg_surface_bool_proc;
        resize : sg_surface_size_proc;
      end;

    sg_input_callbacks = packed record
        do_quit : sg_empty_procedure;
        handle_key_down : sg_intp_proc;
        handle_key_up : sg_intp_proc;
        handle_mouse_up : sg_intp_proc;
        handle_mouse_down : sg_intp_proc;
        handle_input_text : sg_charp_proc;
      end;

    sg_input_interface = packed record
        process_events : sg_empty_procedure;
        window_close_requested : sg_drawing_surface_bool_fn;
        key_pressed : sg_int_intp_fn;
        mouse_state : sg_mouse_state_fn;
        mouse_relative_state : sg_mouse_state_fn;
        mouse_cursor_state : sg_int_intp_fn;
        warp_mouse : sg_surface_xy_proc;

        start_unicode_text_input : sg_rectangle_dimensions_proc;
        stop_unicode_text_input : sg_empty_procedure;
      end;

    sg_text_interface = packed record
        load_font : sg_font_load_fn;
        close_font : sg_font_data_proc;
        text_line_skip : sg_font_int_fn;
        text_size : sg_font_size_fn;
        get_font_style : sg_font_int_fn;
        set_font_style : sg_font_int_proc;
        draw_text : sg_draw_text_proc;
      end;
(* Const before type ignored *)

    sg_interface = packed record
        has_error : longint;
        current_error : ^char;
        init : sg_empty_procedure;
        finalise : sg_empty_procedure;
        audio : sg_audio_interface;
        graphics : sg_graphics_interface;
        image : sg_image_interface;
        input : sg_input_interface;
        text : sg_text_interface;
        utils : sg_utils_interface;
        input_callbacks : sg_input_callbacks;
        read_system_data : sg_system_data_fn;
      end;

  function sg_load(callbacks:sg_input_callbacks): psg_interface; cdecl; external;

  var
  	_sg_functions: ^sg_interface;
    // Currently only a single window... TODO: allow multiple windows
    wind: sg_drawing_surface;
    wind_open: Boolean = false;
    _wind_fullscreen: Boolean = false;
    _wind_border: Boolean = true;
    
  function _ToSGColor(clr: Longint) : sg_color;

implementation


  function _ToSGColor(clr: Longint) : sg_color;
  begin
    result.a := ((clr and $ff000000) shr 24) / 255.0;
    result.r := ((clr and $00ff0000) shr 16) / 255.0;
    result.g := ((clr and $0000ff00) shr  8) / 255.0;
    result.b := ((clr and $000000ff)       ) / 255.0;
  end;
    



end.
