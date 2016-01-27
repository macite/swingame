unit sgDriverSDL2Types;

interface

{$packrecords c}

	//
	// Types from sgBackendTypes.pas
	//
	type
		BytePtr = ^Byte;
		int = longint;
		pint = ^int;
		puint16 = ^uint16;
		float = single;
		pfloat = ^single;
		uint = longword;

		sg_drawing_surface_kind = (
			SGDS_Unknown := 0,
			SGDS_Window  := 1,
			SGDS_Bitmap  := 2 );


		sg_drawing_surface = record
				kind : sg_drawing_surface_kind;
				width : longint;
				height : longint;
				_data : pointer;
			end;

		sg_renderer_flip = (
			SG_FLIP_NONE := 0,
			SG_FLIP_HORIZONTAL := 1,
			SG_FLIP_VERTICAL := 2,
			SG_FLIP_BOTH := 3
			);


		sg_mode = record
				format : uint;
				width : longint;
				height : longint;
				refresh_rate : longint;
			end;

		sg_display = record
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

		sg_audiospec = record
				audio_rate : longint;
				audio_format : longint;
				audio_channels : longint;
				times_opened : longint;
			end;

		sg_system_data = record
				num_displays : dword;
				displays : ^sg_display;
				audio_specs : sg_audiospec;
			end;
		sg_font_kind = (SGFT_UNKNOWN := 0,SGFT_TTF := 1);


		sg_font_data = record
				kind : sg_font_kind;
				_data : pointer;
			end;

		sg_sound_kind = (
			SGSD_UNKNOWN := 0,
			SGSD_SOUND_EFFECT := 1,
			SGSD_MUSIC := 2);

		sg_sound_data = record
				kind : sg_sound_kind;
				// test: longint;
				// test2: longint;
				_data : pointer;
			end;

		sg_window_data = record
		    close_requested: Longint;
		    has_focus: Longint;
		    mouse_over: Longint;
		    shown: Longint;
    	end;

		sg_font_style = (SG_FONT_STYLE_NORMAL := 0,SG_FONT_STYLE_BOLD := 1,
			SG_FONT_STYLE_ITALIC := 2,SG_FONT_STYLE_UNDERLINE := 4,
			SG_FONT_STYLE_STRIKETHROUGH := 8);

		sg_connection_kind = ( SGCK_UNKNOWN = 0, SGCK_TCP = 1, SGCK_UDP = 2);

		sg_network_connection = record
			kind : sg_connection_kind;
			_socket: pointer;
		end;

	//
	// sgInterface types
	//
	type
		puint = ^uint;

		sg_color = record
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

		sg_drawing_surface_string_bool_fn = function(surface: psg_drawing_surface; filename: pchar): int; cdecl; // (surface, string) -> bool

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

		sg_sound_load_fn = function(name: PChar; kind: sg_sound_kind): sg_sound_data; cdecl;

		sg_sound_fn = function() : psg_sound_data;

		//
		// Utility related
		//
		sg_uint_fn = function(): uint; cdecl;

		// 
		// Image related
		//
		sg_load_surface_fn = function(title: pchar): sg_drawing_surface; cdecl;
		sg_drawing_surface_surface_proc = procedure(src, dst: psg_drawing_surface; src_data: pfloat; src_data_sz: int; dst_data: pfloat; dst_data_sz:int; flip: sg_renderer_flip ); cdecl;

		//
		// Network related
		//
		psg_network_connection = ^sg_network_connection;

		sg_create_network_fn = function(host: pchar; port: uint16): sg_network_connection; cdecl;
		sg_create_udp_network_fn = function(port: uint16): sg_network_connection; cdecl;
    	sg_network_data_fn = function(connection: psg_network_connection; buffer: BytePtr; size: Longint): Longint; cdecl;
    	sg_connection_fn = procedure (connection: psg_network_connection); cdecl;
    	sg_connection_uint_fn = function (connection: psg_network_connection): uint; cdecl;
    	sg_accept_connection_fn = function (connection: psg_network_connection): sg_network_connection; cdecl;
    	sg_udp_send_fn = function (con: psg_network_connection; host: pchar; port: uint16; buffer: pchar; size: LongInt): LongInt; cdecl;
		sg_read_udp_fn = procedure (con: psg_network_connection; host: puint; port: puint16; buffer: pchar; size: puint); cdecl;


		//
		// Input related
		//
		sg_mouse_state_fn = function(x, y: pint): uint; cdecl;
		sg_window_pos_fn = procedure (surface: psg_drawing_surface; x, y: pint); cdecl;
		sg_surface_xy_proc = procedure(surface: psg_drawing_surface; x, y: int); cdecl;
	    pointer_fn = function (): pointer; cdecl;
    	sg_window_xy_proc = procedure(window: pointer; x, y: Longint); cdecl;
    	sg_window_data_fn = function(surface: psg_drawing_surface): sg_window_data; cdecl;
		
		sg_utils_interface = record
				delay : sg_single_uint32param_proc;
				get_ticks : sg_uint_fn;
			end;

		sg_image_interface = record
				create_bitmap : sg_create_surface_fn;
				load_bitmap : sg_load_surface_fn;
				draw_bitmap : sg_drawing_surface_surface_proc;
			end;

		sg_audio_interface = record
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
				music_playing : sg_float_fn;
				current_music : sg_sound_fn;
			end;

		sg_graphics_interface = record
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
				save_png: sg_drawing_surface_string_bool_fn;
			end;

		sg_input_callbacks = record
				do_quit : sg_empty_procedure;
				handle_key_down : sg_intp_proc;
				handle_key_up : sg_intp_proc;
				handle_mouse_up : sg_intp_proc;
				handle_mouse_down : sg_intp_proc;
				handle_input_text : sg_charp_proc;
				handle_window_resize: sg_window_xy_proc;
				handle_window_move: sg_window_xy_proc;
			end;

		sg_input_interface = record
				process_events : sg_empty_procedure;
				window_close_requested : sg_drawing_surface_bool_fn;
				key_pressed : sg_int_intp_fn;
				mouse_state : sg_mouse_state_fn;
				mouse_relative_state : sg_mouse_state_fn;
				mouse_cursor_state : sg_int_intp_fn;
				warp_mouse : sg_surface_xy_proc;

				start_unicode_text_input : sg_rectangle_dimensions_proc;
				stop_unicode_text_input : sg_empty_procedure;

				focus_window: pointer_fn;
        		window_position: sg_window_pos_fn;
        		get_window_event_data: sg_window_data_fn;
        		move_window: sg_surface_xy_proc;
			end;

		sg_text_interface = record
				load_font : sg_font_load_fn;
				close_font : sg_font_data_proc;
				text_line_skip : sg_font_int_fn;
				text_size : sg_font_size_fn;
				get_font_style : sg_font_int_fn;
				set_font_style : sg_font_int_proc;
				draw_text : sg_draw_text_proc;
			end;

		sg_network_interface = record
			open_tcp_connection: 	sg_create_network_fn;
			open_udp_connection: 	sg_create_udp_network_fn;
			read_bytes: 			sg_network_data_fn;
			send_bytes: 			sg_network_data_fn;
        	close_connection: 		sg_connection_fn;
        	network_address: 		sg_connection_uint_fn;
        	network_port: 			sg_connection_uint_fn;
        	accept_new_connection: 	sg_accept_connection_fn;
        	network_has_data:		sg_uint_fn;
        	connection_has_data:	sg_connection_uint_fn;
        	send_udp_message:		sg_udp_send_fn;
        	read_udp_message:		sg_read_udp_fn;
		end;

(* Const before type ignored *)

	sg_http_method = (
	        HTTP_GET,
	        HTTP_POST,
	        HTTP_PUT,
	        HTTP_DELETE
    	);
    
    sg_http_request = record
        request_type: sg_http_method;
        url: PChar;
        port: Word;
        body: PChar;
    end;
    
    sg_http_response = record
        status: Word;
        size: uint;
        data: PChar;
    end;

    psg_http_response = ^sg_http_response;

    sg_http_request_fn = function (request: sg_http_request): sg_http_response; cdecl;
    sg_free_response_fn = procedure (response: psg_http_response); cdecl;
    
    sg_web_interface = record
        http_request: sg_http_request_fn;
        free_response: sg_free_response_fn;
    end;


		sg_interface = record
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
				network : sg_network_interface;
				web : sg_web_interface;
				input_callbacks : sg_input_callbacks;
				read_system_data : sg_system_data_fn;
			end;

	function sg_load(callbacks: sg_input_callbacks): psg_interface; cdecl; external 'libsgsdl2';

	var
		_sg_functions: ^sg_interface;
		
	function _ToSGColor(clr: Longint) : sg_color;


	const
		SDLK_RETURN = $D;
		SDLK_ESCAPE = $1B;
		SDLK_BACKSPACE = $8;
		SDLK_TAB = $9;
		SDLK_SPACE = Ord(' ');
		SDLK_EXCLAIM = Ord('!');
		SDLK_QUOTEDBL = Ord('"');
		SDLK_HASH = Ord('#');
		SDLK_DOLLAR = Ord('$');
		SDLK_PERCENT = Ord('%');
		SDLK_AMPERSAND = Ord('&');
		SDLK_QUOTE = Ord('''');
		SDLK_LEFTPAREN = Ord('(');
		SDLK_RIGHTPAREN = Ord(')');
		SDLK_ASTERISK = Ord('*');
		SDLK_PLUS = Ord('+');
		SDLK_COMMA = Ord(',');
		SDLK_MINUS = Ord('-');
		SDLK_PERIOD = Ord('.');
		SDLK_SLASH = Ord('/');
		SDLK_0 = Ord('0');
		SDLK_1 = Ord('1');
		SDLK_2 = Ord('2');
		SDLK_3 = Ord('3');
		SDLK_4 = Ord('4');
		SDLK_5 = Ord('5');
		SDLK_6 = Ord('6');
		SDLK_7 = Ord('7');
		SDLK_8 = Ord('8');
		SDLK_9 = Ord('9');
		SDLK_COLON = Ord(':');
		SDLK_SEMICOLON = Ord(';');
		SDLK_LESS = Ord('<');
		SDLK_EQUALS = Ord('=');
		SDLK_GREATER = Ord('>');
		SDLK_QUESTION = Ord('?');
		SDLK_AT = Ord('@');
		SDLK_LEFTBRACKET = Ord('[');
		SDLK_BACKSLASH = Ord('\');
		SDLK_RIGHTBRACKET = Ord(']');
		SDLK_CARET = Ord('^');
		SDLK_UNDERSCORE = Ord('_');
		SDLK_BACKQUOTE = Ord('`');
		SDLK_a = Ord('a');
		SDLK_b = Ord('b');
		SDLK_c = Ord('c');
		SDLK_d = Ord('d');
		SDLK_e = Ord('e');
		SDLK_f = Ord('f');
		SDLK_g = Ord('g');
		SDLK_h = Ord('h');
		SDLK_i = Ord('i');
		SDLK_j = Ord('j');
		SDLK_k = Ord('k');
		SDLK_l = Ord('l');
		SDLK_m = Ord('m');
		SDLK_n = Ord('n');
		SDLK_o = Ord('o');
		SDLK_p = Ord('p');
		SDLK_q = Ord('q');
		SDLK_r = Ord('r');
		SDLK_s = Ord('s');
		SDLK_t = Ord('t');
		SDLK_u = Ord('u');
		SDLK_v = Ord('v');
		SDLK_w = Ord('w');
		SDLK_x = Ord('x');
		SDLK_y = Ord('y');
		SDLK_z = Ord('z');
		SDLK_CAPSLOCK = $40000039;
		SDLK_F1 = $4000003A;
		SDLK_F2 = $4000003B;
		SDLK_F3 = $4000003C;
		SDLK_F4 = $4000003D;
		SDLK_F5 = $4000003E;
		SDLK_F6 = $4000003F;
		SDLK_F7 = $40000040;
		SDLK_F8 = $40000041;
		SDLK_F9 = $40000042;
		SDLK_F10 = $40000043;
		SDLK_F11 = $40000044;
		SDLK_F12 = $40000045;
		SDLK_PRINTSCREEN = $40000046;
		SDLK_SCROLLLOCK = $40000047;
		SDLK_PAUSE = $40000048;
		SDLK_INSERT = $40000049;
		SDLK_HOME = $4000004A;
		SDLK_PAGEUP = $4000004B;
		SDLK_DELETE = $7F;
		SDLK_END = $4000004D;
		SDLK_PAGEDOWN = $4000004E;
		SDLK_RIGHT = $4000004F;
		SDLK_LEFT = $40000050;
		SDLK_DOWN = $40000051;
		SDLK_UP = $40000052;
		SDLK_NUMLOCKCLEAR = $40000053;
		SDLK_KP_DIVIDE = $40000054;
		SDLK_KP_MULTIPLY = $40000055;
		SDLK_KP_MINUS = $40000056;
		SDLK_KP_PLUS = $40000057;
		SDLK_KP_ENTER = $40000058;
		SDLK_KP_1 = $40000059;
		SDLK_KP_2 = $4000005A;
		SDLK_KP_3 = $4000005B;
		SDLK_KP_4 = $4000005C;
		SDLK_KP_5 = $4000005D;
		SDLK_KP_6 = $4000005E;
		SDLK_KP_7 = $4000005F;
		SDLK_KP_8 = $40000060;
		SDLK_KP_9 = $40000061;
		SDLK_KP_0 = $40000062;
		SDLK_KP_PERIOD = $40000063;
		SDLK_APPLICATION = $40000065;
		SDLK_POWER = $40000066;
		SDLK_KP_EQUALS = $40000067;
		SDLK_F13 = $40000068;
		SDLK_F14 = $40000069;
		SDLK_F15 = $4000006A;
		SDLK_F16 = $4000006B;
		SDLK_F17 = $4000006C;
		SDLK_F18 = $4000006D;
		SDLK_F19 = $4000006E;
		SDLK_F20 = $4000006F;
		SDLK_F21 = $40000070;
		SDLK_F22 = $40000071;
		SDLK_F23 = $40000072;
		SDLK_F24 = $40000073;
		SDLK_EXECUTE = $40000074;
		SDLK_HELP = $40000075;
		SDLK_MENU = $40000076;
		SDLK_SELECT = $40000077;
		SDLK_STOP = $40000078;
		SDLK_AGAIN = $40000079;
		SDLK_UNDO = $4000007A;
		SDLK_CUT = $4000007B;
		SDLK_COPY = $4000007C;
		SDLK_PASTE = $4000007D;
		SDLK_FIND = $4000007E;
		SDLK_MUTE = $4000007F;
		SDLK_VOLUMEUP = $40000080;
		SDLK_VOLUMEDOWN = $40000081;
		SDLK_KP_COMMA = $40000085;
		SDLK_KP_EQUALSAS400 = $40000086;
		SDLK_ALTERASE = $40000099;
		SDLK_SYSREQ = $4000009A;
		SDLK_CANCEL = $4000009B;
		SDLK_CLEAR = $4000009C;
		SDLK_PRIOR = $4000009D;
		SDLK_RETURN2 = $4000009E;
		SDLK_SEPARATOR = $4000009F;
		SDLK_OUT = $400000A0;
		SDLK_OPER = $400000A1;
		SDLK_CLEARAGAIN = $400000A2;
		SDLK_CRSEL = $400000A3;
		SDLK_EXSEL = $400000A4;
		SDLK_KP_00 = $400000B0;
		SDLK_KP_000 = $400000B1;
		SDLK_THOUSANDSSEPARATOR = $400000B2;
		SDLK_DECIMALSEPARATOR = $400000B3;
		SDLK_CURRENCYUNIT = $400000B4;
		SDLK_CURRENCYSUBUNIT = $400000B5;
		SDLK_KP_LEFTPAREN = $400000B6;
		SDLK_KP_RIGHTPAREN = $400000B7;
		SDLK_KP_LEFTBRACE = $400000B8;
		SDLK_KP_RIGHTBRACE = $400000B9;
		SDLK_KP_TAB = $400000BA;
		SDLK_KP_BACKSPACE = $400000BB;
		SDLK_KP_A = $400000BC;
		SDLK_KP_B = $400000BD;
		SDLK_KP_C = $400000BE;
		SDLK_KP_D = $400000BF;
		SDLK_KP_E = $400000C0;
		SDLK_KP_F = $400000C1;
		SDLK_KP_XOR = $400000C2;
		SDLK_KP_POWER = $400000C3;
		SDLK_KP_PERCENT = $400000C4;
		SDLK_KP_LESS = $400000C5;
		SDLK_KP_GREATER = $400000C6;
		SDLK_KP_AMPERSAND = $400000C7;
		SDLK_KP_DBLAMPERSAND = $400000C8;
		SDLK_KP_VERTICALBAR = $400000C9;
		SDLK_KP_DBLVERTICALBAR = $400000CA;
		SDLK_KP_COLON = $400000CB;
		SDLK_KP_HASH = $400000CC;
		SDLK_KP_SPACE = $400000CD;
		SDLK_KP_AT = $400000CE;
		SDLK_KP_EXCLAM = $400000CF;
		SDLK_KP_MEMSTORE = $400000D0;
		SDLK_KP_MEMRECALL = $400000D1;
		SDLK_KP_MEMCLEAR = $400000D2;
		SDLK_KP_MEMADD = $400000D3;
		SDLK_KP_MEMSUBTRACT = $400000D4;
		SDLK_KP_MEMMULTIPLY = $400000D5;
		SDLK_KP_MEMDIVIDE = $400000D6;
		SDLK_KP_PLUSMINUS = $400000D7;
		SDLK_KP_CLEAR = $400000D8;
		SDLK_KP_CLEARENTRY = $400000D9;
		SDLK_KP_BINARY = $400000DA;
		SDLK_KP_OCTAL = $400000DB;
		SDLK_KP_DECIMAL = $400000DC;
		SDLK_KP_HEXADECIMAL = $400000DD;
		SDLK_LCTRL = $400000E0;
		SDLK_LSHIFT = $400000E1;
		SDLK_LALT = $400000E2;
		SDLK_LGUI = $400000E3;
		SDLK_RCTRL = $400000E4;
		SDLK_RSHIFT = $400000E5;
		SDLK_RALT = $400000E6;
		SDLK_RGUI = $400000E7;
		SDLK_MODE = $40000101;
		SDLK_AUDIONEXT = $40000102;
		SDLK_AUDIOPREV = $40000103;
		SDLK_AUDIOSTOP = $40000104;
		SDLK_AUDIOPLAY = $40000105;
		SDLK_AUDIOMUTE = $40000106;
		SDLK_MEDIASELECT = $40000107;
		SDLK_WWW = $40000108;
		SDLK_MAIL = $40000109;
		SDLK_CALCULATOR = $4000010A;
		SDLK_COMPUTER = $4000010B;
		SDLK_AC_SEARCH = $4000010C;
		SDLK_AC_HOME = $4000010D;
		SDLK_AC_BACK = $4000010E;
		SDLK_AC_FORWARD = $4000010F;
		SDLK_AC_STOP = $40000110;
		SDLK_AC_REFRESH = $40000111;
		SDLK_AC_BOOKMARKS = $40000112;
		SDLK_BRIGHTNESSDOWN = $40000113;
		SDLK_BRIGHTNESSUP = $40000114;
		SDLK_DISPLAYSWITCH = $40000115;
		SDLK_KBDILLUMTOGGLE = $40000116;
		SDLK_KBDILLUMDOWN = $40000117;
		SDLK_KBDILLUMUP = $40000118;
		SDLK_EJECT = $40000119;
		SDLK_SLEEP = $4000011A;

implementation


	function _ToSGColor(clr: Longint) : sg_color;
	begin
		result.a := ((clr and $ff000000) shr 24) / 255.0;
		result.r := ((clr and $00ff0000) shr 16) / 255.0;
		result.g := ((clr and $0000ff00) shr  8) / 255.0;
		result.b := ((clr and $000000ff)       ) / 255.0;
	end;
		



end.
