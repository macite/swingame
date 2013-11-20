//
//  test_draw_point.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "test_draw_point.h"

#include "SDL.h"

//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;


//The window we'll be rendering to
SDL_Window* gWindow = NULL;

//The window renderer
SDL_Renderer* gRenderer = NULL;

bool init()
{
	//Initialization flag
	bool success = true;
    
	//Initialize SDL
	if( SDL_Init( SDL_INIT_EVERYTHING ) < 0 )
	{
		printf( "SDL could not initialize! SDL Error: %s\n", SDL_GetError() );
		success = false;
	}
	else
	{
		//Set texture filtering to linear
//		if( !SDL_SetHint( SDL_HINT_RENDER_SCALE_QUALITY, "1" ) )
//		{
//			printf( "Warning: Linear texture filtering not enabled!" );
//		}
        
        SDL_GL_SetAttribute(SDL_GL_RED_SIZE,            8);
        SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,          8);
        SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,           8);
        SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,          8);
        SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,        1);
        SDL_GL_SetAttribute(SDL_GL_BUFFER_SIZE,         32);
        SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,          16);
        SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE,        0);
        SDL_GL_SetAttribute(SDL_GL_ACCUM_RED_SIZE,      0);
        SDL_GL_SetAttribute(SDL_GL_ACCUM_GREEN_SIZE,    0);
        SDL_GL_SetAttribute(SDL_GL_ACCUM_BLUE_SIZE,     0);
        SDL_GL_SetAttribute(SDL_GL_ACCUM_ALPHA_SIZE,    0);
        SDL_GL_SetAttribute(SDL_GL_STEREO,              0);
        SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,  0);
        SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,  0);
        
        SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL,  1);
        
        SDL_GL_SetAttribute(SDL_GL_RETAINED_BACKING,    1);

        
		//Create window
		gWindow = SDL_CreateWindow( "SDL Tutorial", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN );
        
		if( gWindow == NULL )
		{
			printf( "Window could not be created! SDL Error: %s\n", SDL_GetError() );
			success = false;
		}
		else
		{
			//Create renderer for window
			gRenderer = SDL_CreateRenderer( gWindow, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC );
            
			if( gRenderer == NULL )
			{
				printf( "Renderer could not be created! SDL Error: %s\n", SDL_GetError() );
				success = false;
			}
			else
			{
				//Initialize renderer color
				SDL_SetRenderDrawColor( gRenderer, 120, 120, 120, 255 );
                SDL_RenderClear(gRenderer);
                SDL_RenderPresent(gRenderer);
                SDL_Delay(500);

                
				//Initialize PNG loading
//				int imgFlags = IMG_INIT_PNG;
//				if( !( IMG_Init( imgFlags ) & imgFlags ) )
//				{
//					printf( "SDL_image could not initialize! SDL_image Error: %s\n", IMG_GetError() );
//					success = false;
//				}
			}
		}
	}
    
	return success;
}

void close()
{
	//Destroy window
	SDL_DestroyRenderer( gRenderer );
	SDL_DestroyWindow( gWindow );
	gWindow = NULL;
	gRenderer = NULL;
    
	//Quit SDL subsystems
	SDL_Quit();
}

void test_draw_point( )
{
	//Start up SDL and create window
	if( !init() )
	{
		printf( "Failed to initialize!\n" );
	}
	else
	{
			//Main loop flag
			bool quit = false;
            
			//Event handler
//			SDL_Event e;
            
            int a = 0;
			//While application is running
        
            SDL_SetRenderDrawColor( gRenderer, 0xFF, 0xFF, 0xFF, 0xFF );
            SDL_RenderClear( gRenderer );

            while( a < 100 )
			{
                a++;
				//Handle events on queue
//				while( SDL_PollEvent( &e ) != 0 )
//				{
//					//User requests quit
//					if( e.type == SDL_QUIT )
//					{
//						quit = true;
//					}
//				}
//                
				//Clear screen
                
//				//Render red filled quad
//				SDL_Rect fillRect = { SCREEN_WIDTH / 4, SCREEN_HEIGHT / 4, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 };
//				SDL_SetRenderDrawColor( gRenderer, 0xFF, 0x00, 0x00, 0xFF );
//				SDL_RenderFillRect( gRenderer, &fillRect );
//
//				//Render green outlined quad
//				SDL_Rect outlineRect = { SCREEN_WIDTH / 6, SCREEN_HEIGHT / 6, SCREEN_WIDTH * 2 / 3, SCREEN_HEIGHT * 2 / 3 };
//				SDL_SetRenderDrawColor( gRenderer, 0x00, 0xFF, 0x00, 0xFF );
//				SDL_RenderDrawRect( gRenderer, &outlineRect );
//				
//				//Draw blue horizontal line
//				SDL_SetRenderDrawColor( gRenderer, 0x00, 0x00, 0xFF, 0xFF );
//				SDL_RenderDrawLine( gRenderer, 0, SCREEN_HEIGHT / 2, SCREEN_WIDTH, SCREEN_HEIGHT / 2 );
                
				//Draw vertical line of yellow dots
				SDL_SetRenderDrawColor( gRenderer, 0x00, 0x00, 0x00, 0xFF );
				for( int i = 0; i < 1000; i ++ )
				{
					SDL_RenderDrawPoint( gRenderer, rand() % SCREEN_WIDTH, rand() % SCREEN_HEIGHT );
				}
                
				//Update screen
				SDL_RenderPresent( gRenderer );
		}
	}
    
	//Free resources and close SDL
	close();
    
}

