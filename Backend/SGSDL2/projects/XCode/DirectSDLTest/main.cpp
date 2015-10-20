//
//  main.cpp
//  DirectSDLTest
//
//  Created by Andrew Cain on 21/10/2015.
//  Copyright © 2015 Andrew Cain. All rights reserved.
//

#include <iostream>
#include <SDL.h>

int main(int argc, const char * argv[]) {
    // insert code here...
    SDL_SetMainReady();
    
    if ( -1 == SDL_Init( SDL_INIT_EVERYTHING ) )
    {
        // fatal error so...
        // no other functions can now be called
        return -1;
    }
    
    SDL_SetHint(SDL_HINT_RENDER_DRIVER, "opengl");
    
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE,    8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,  8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,   8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,  8);
    
    SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL,  1);
    
    SDL_Window *window = SDL_CreateWindow("Test Window",
                                         SDL_WINDOWPOS_CENTERED,
                                         SDL_WINDOWPOS_CENTERED,
                                         800,
                                         600,
                                         SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL);
    
    printf("Open window %p\n", window);
    
    if ( ! window )
    {
        return -1;
    }
    
    SDL_DisplayMode fullscreen_mode;
    SDL_zero(fullscreen_mode);
    fullscreen_mode.format = SDL_PIXELFORMAT_RGB888;
    fullscreen_mode.refresh_rate = 60;
    SDL_SetWindowDisplayMode(window, &fullscreen_mode);
    
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, 0);
    
    //std::cout << "Renderer is " << window_be->renderer << std::endl;
    
    SDL_SetRenderDrawColor(renderer, 120, 120, 120, 255);
    SDL_RenderClear(renderer);
    SDL_RenderPresent(renderer);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
    
    //    window_be->backing = SDL_CreateTexture(window_be->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
    //
    //    SDL_SetRenderTarget(window_be->renderer, window_be->backing);
    //    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    //    SDL_RenderClear(window_be->renderer);
    
//    SDL_ShowWindow(window_be->window);
    bool fullscreen = false;
    
    SDL_PumpEvents();
    
    SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN : SDL_FALSE);
    
    SDL_SetRenderDrawColor(renderer,
                           static_cast<Uint8>(1.0 * 255),
                           static_cast<Uint8>(1.0 * 255),
                           static_cast<Uint8>(1.0 * 255),
                           static_cast<Uint8>(1.0 * 255));
    SDL_RenderClear(renderer);

    SDL_Rect rect = { 0, 0, 1, 1 };
    SDL_RenderFillRect(renderer, &rect);

    SDL_SetRenderTarget(renderer, NULL);
    SDL_RenderPresent(renderer);
    SDL_PumpEvents();
    
    SDL_Delay(2000);
    
    fullscreen = true;
    
    SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN : SDL_FALSE);
    
    SDL_SetRenderDrawColor(renderer,
                           static_cast<Uint8>(1.0 * 255),
                           static_cast<Uint8>(0.0 * 255),
                           static_cast<Uint8>(0.0 * 255),
                           static_cast<Uint8>(1.0 * 255));
    SDL_RenderClear(renderer);
    
    SDL_RenderFillRect(renderer, &rect);

    SDL_SetRenderTarget(renderer, NULL);
    SDL_RenderPresent(renderer);
    SDL_PumpEvents();
    
    SDL_Delay(2000);
    
    fullscreen = false;
    
    SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN : SDL_FALSE);
    
    SDL_SetRenderDrawColor(renderer,
                           static_cast<Uint8>(0.0 * 255),
                           static_cast<Uint8>(1.0 * 255),
                           static_cast<Uint8>(0.0 * 255),
                           static_cast<Uint8>(1.0 * 255));
    SDL_RenderClear(renderer);
    
    SDL_RenderFillRect(renderer, &rect);
    
    SDL_SetRenderTarget(renderer, NULL);
    SDL_RenderPresent(renderer);
    
    SDL_Delay(2000);
    
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    
    return 0;
}
