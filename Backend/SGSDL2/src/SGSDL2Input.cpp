//
//  SGSDL2Input.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 7/12/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Input.h"
#include <SDL.h>

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
            window->shown = true;
            //SDL_Log("Window %d shown", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_HIDDEN:
            window->shown = false;
//            SDL_Log("Window %d hidden", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_EXPOSED:
            SDL_Log("Window %d exposed", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_MOVED:
            SDL_Log("Window %d moved to %d,%d",
                    event->window.windowID, event->window.data1,
                    event->window.data2);
            break;
        case SDL_WINDOWEVENT_RESIZED:
            SDL_Log("Window %d resized to %dx%d",
                    event->window.windowID, event->window.data1,
                    event->window.data2);
            break;
        case SDL_WINDOWEVENT_MINIMIZED:
            SDL_Log("Window %d minimized", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_MAXIMIZED:
            SDL_Log("Window %d maximized", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_RESTORED:
            SDL_Log("Window %d restored", event->window.windowID);
            break;
        case SDL_WINDOWEVENT_ENTER:
            window->mouse_over = true;
//            SDL_Log("Mouse entered window %d",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_LEAVE:
//            SDL_Log("Mouse left window %d", event->window.windowID);
            window->mouse_over = false;
            break;
        case SDL_WINDOWEVENT_FOCUS_GAINED:
            window->has_focus = true;
//            SDL_Log("Window %d gained keyboard focus",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_FOCUS_LOST:
            window->has_focus = false;
//            SDL_Log("Window %d lost keyboard focus",
//                    event->window.windowID);
            break;
        case SDL_WINDOWEVENT_CLOSE:
//            SDL_Log("Window %d closed", event->window.windowID);
            window->close_requested = true;
            break;
        default:
            SDL_Log("Window %d got unknown event %d",
                    event->window.windowID, event->window.event);
            break;
    }
}

void sgsdl2_process_events()
{
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
            
        }
    }
}

int sgsdl2_window_close_requested(sg_drawing_surface* surf)
{
  if (surf->kind == SGDS_Window) 
  {
    if (((sg_window_be*)surf->_data)->close_requested)
    {
      return -1;
    }
  }
  return 0;
}

int sgsdl2_key_pressed(int key_code)
{
    const Uint8 *keys;
    int key_scancode = SDL_GetScancodeFromKey(key_code);
    int sz;

    keys = SDL_GetKeyboardState(&sz);
    
    if ( (! keys) || sz <= key_scancode ) return 0;
    
    if ( keys[key_scancode] == 1 ) return -1;
    else return 0;
}

void sgsdl2_load_input_fns(sg_interface *functions)
{
    functions->input.process_events = & sgsdl2_process_events;
    functions->input.window_close_requested = & sgsdl2_window_close_requested;
    functions->input.key_pressed= &sgsdl2_key_pressed;
    functions->input.mouse_state = &SDL_GetMouseState;  // call it directly
    functions->input.mouse_relative_state = &SDL_GetRelativeMouseState;
}
