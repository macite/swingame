//
//  SGSDL2Input.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 7/12/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifdef __linux__
#include <SDL2/SDL.h>
#else
#include <SDL.h>
#endif

#include "SGSDL2Core.h"
#include "SGSDL2Input.h"
#include "SGSDL2Graphics.h"

#include "sgBackendUtils.h"

void _sgsdl2_handle_window_event(SDL_Event * event)
{
    sg_window_be *window = _sgsdl2_get_window_with_id(event->window.windowID);

    if (! window) {
        return;
    }

    switch (event->window.event)
    {
        case SDL_WINDOWEVENT_SHOWN:
            window->event_data.shown = true;
            //SDL_Log("Window %d shown", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_HIDDEN:
            window->event_data.shown = false;
//            SDL_Log("Window %d hidden", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_EXPOSED:
//            SDL_Log("Window %d exposed", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_MOVED:
//            SDL_Log("Window %d moved to %d,%d",
//                    event->window.windowID, event->window.data1,
//                    event->window.data2);
            if (_functions.input_callbacks.handle_window_move)
            {
                _functions.input_callbacks.handle_window_move(_sgsdl2_get_window_with_id(event->window.windowID), event->window.data1, event->window.data2);
            }

            break;
        case SDL_WINDOWEVENT_RESIZED:
            if (_functions.input_callbacks.handle_window_resize)
            {
                _functions.input_callbacks.handle_window_resize(_sgsdl2_get_window_with_id(event->window.windowID), event->window.data1, event->window.data2);
            }

            break;
        case SDL_WINDOWEVENT_MINIMIZED:
//            SDL_Log("Window %d minimized", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_MAXIMIZED:
//            SDL_Log("Window %d maximized", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_RESTORED:
//            SDL_Log("Window %d restored", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_ENTER:
            window->event_data.mouse_over = true;
//            SDL_Log("Mouse entered window %d",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_LEAVE:
//            SDL_Log("Mouse left window %d", event->window.windowID);
            window->event_data.mouse_over = false;
            break;
        case SDL_WINDOWEVENT_FOCUS_GAINED:
            window->event_data.has_focus = true;
//            SDL_Log("Window %d gained keyboard focus",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_FOCUS_LOST:
            window->event_data.has_focus = false;
//            SDL_Log("Window %d lost keyboard focus",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_CLOSE:
//            SDL_Log("Window %d closed", event->window.windowID);
            window->event_data.close_requested = true;
            break;
        default:
//            SDL_Log("Window %d got unknown event %d",
//                    event->window.windowID, event->window.event);
            break;
    }
}

void sgsdl2_process_events()
{
    internal_sgsdl2_init();
    SDL_Event event;

    while (SDL_WaitEventTimeout(&event, 0))
    {
        switch ( event.type )
        {
            case SDL_WINDOWEVENT:
            {
                _sgsdl2_handle_window_event(&event);
                break;
            }

            case SDL_QUIT:
            {
                // Use callback to inform front end of quit
                if (_functions.input_callbacks.do_quit)
                {
                    _functions.input_callbacks.do_quit();
                }
                break;
            }

            case SDL_KEYDOWN:
            {
                if (_functions.input_callbacks.handle_key_down)
                {
                    int key_code = static_cast<int>(event.key.keysym.sym);
                    _functions.input_callbacks.handle_key_down(key_code);
                }
                break;
            }

            case SDL_KEYUP:
            {
                if (_functions.input_callbacks.handle_key_up)
                {
                    int key_code = static_cast<int>(event.key.keysym.sym);
                    _functions.input_callbacks.handle_key_up(key_code);
                }
                break;
            }

            case SDL_MOUSEBUTTONUP:
            {
                if (_functions.input_callbacks.handle_mouse_up)
                {
                    int mouse_button = event.button.button;
                    _functions.input_callbacks.handle_mouse_up(mouse_button);
                }
                break;
            }

            case SDL_MOUSEBUTTONDOWN:
            {
                if (_functions.input_callbacks.handle_mouse_down)
                {
                    int mouse_button = event.button.button;
                    _functions.input_callbacks.handle_mouse_down(mouse_button);
                }
                break;
            }
            
            case SDL_MOUSEWHEEL:
            {
                if (_functions.input_callbacks.handle_mouse_wheel)
                {
                    _functions.input_callbacks.handle_mouse_wheel(event.wheel.x, event.wheel.y);
                }
                break;
            }
            
            case SDL_TEXTEDITING:
            {
                if (_functions.input_callbacks.handle_input_text)
                {
                  char* text = event.edit.text;
                  _functions.input_callbacks.handle_input_text(text);
                }
                break;
            }
            case SDL_TEXTINPUT:
            {
                if (_functions.input_callbacks.handle_input_text)
                {
                  char* text = event.text.text;
                  _functions.input_callbacks.handle_input_text(text);
                }
                break;
            }

        }
    }
}

int sgsdl2_window_close_requested(sg_drawing_surface* surf)
{
  if (surf->kind == SGDS_Window)
  {
    if (static_cast<sg_window_be*>(surf->_data)->event_data.close_requested)
    {
      return -1;
    }
  }
  return 0;
}

int sgsdl2_key_pressed(int key_code)
{
    internal_sgsdl2_init();

    const Uint8 *keys;
    int key_scancode = SDL_GetScancodeFromKey(key_code);
    int sz;

    keys = SDL_GetKeyboardState(&sz);

    if ( (! keys) || sz <= key_scancode ) return 0;

    if ( keys[key_scancode] == 1 ) return -1;
    else return 0;
}



void sgsdl2_start_unicode_text_input(int x, int y, int w, int h)
{
    internal_sgsdl2_init();
    SDL_Rect rect = {x,y,w,h};
    SDL_SetTextInputRect(&rect);

    SDL_StartTextInput();
}

void sgsdl2_warp_mouse(sg_drawing_surface *surface, int x, int y)
{
    if ( ! surface || ! surface->_data ) return;

    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = static_cast<sg_window_be *>(surface->_data);

            SDL_WarpMouseInWindow(window_be->window, x, y);
            break;
        }

        case SGDS_Bitmap:
            break;

        case SGDS_Unknown:
            break;
    }
}

pointer sgsdl2_focus_window()
{
    internal_sgsdl2_init();
    return _sgsdl2_get_window_with_pointer(SDL_GetMouseFocus());
}

void sgsdl2_window_position(sg_drawing_surface *surface, int *x, int *y)
{
    if ( ! surface || ! surface->_data ) return;

    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;

            SDL_GetWindowPosition(window_be->window, x, y);
            break;
        }

        case SGDS_Bitmap:
            break;

        case SGDS_Unknown:
            break;
    }
}

sg_window_data sgsdl2_get_window_event_data(sg_drawing_surface *surface)
{
    sg_window_data result = { 0, 0, 0, 0 };

    if ( ! surface || ! surface->_data ) return result;

    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;

            return window_be->event_data;
        }

        default:
            return result;
    }
}

void sgsdl2_move_window(sg_drawing_surface *surface, int x, int y)
{
    if ( ! surface || ! surface->_data ) return;

    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;

            SDL_SetWindowPosition(window_be->window, x, y);

            return;
        }

        default: ;
    }
}


void sgsdl2_load_input_fns(sg_interface *functions)
{
    functions->input.process_events = & sgsdl2_process_events;
    functions->input.window_close_requested = & sgsdl2_window_close_requested;
    functions->input.key_pressed= &sgsdl2_key_pressed;
    functions->input.mouse_state = &SDL_GetMouseState;  // call it directly
    functions->input.mouse_relative_state = &SDL_GetRelativeMouseState;
    functions->input.mouse_cursor_state = &SDL_ShowCursor; // 0 hide, 1 show, -1 query
    functions->input.start_unicode_text_input = &sgsdl2_start_unicode_text_input;
    functions->input.stop_unicode_text_input = &SDL_StopTextInput;
    functions->input.warp_mouse = &sgsdl2_warp_mouse;
    functions->input.focus_window = &sgsdl2_focus_window;
    functions->input.window_position = &sgsdl2_window_position;
    functions->input.get_window_event_data = &sgsdl2_get_window_event_data;
    functions->input.move_window = &sgsdl2_move_window;
}
