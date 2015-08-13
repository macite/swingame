//
//  SGSDL2Texture.cpp
//  sgsdl2
//
//  Created by James Ferguson on 30/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//


#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#else
#include <SDL.h>
#include <SDL_image.h>
#endif

#include "SGSDL2Texture.h"
#include "SGSDL2Utilities.h"


sgsdl2_texture* sgsdl2_create_texture(sgsdl2_scene *scene)
{
	sgsdl2_texture *texture = new sgsdl2_texture();
	scene->textures.push_back(texture);
	glGenTextures(1, &texture->handle);
	return texture;
}

sgsdl2_texture* sgsdl2_create_texture(sgsdl2_scene *scene, const char *image_path)
{
	sgsdl2_texture *tex = sgsdl2_create_texture(scene);
	sgsdl2_attach_texture_image(tex, image_path);
	return tex;
}

sgsdl2_texture* sgsdl2_find_or_create_texture(sgsdl2_scene *scene, const char *image_path)
{
	// Check to see if the image has been loaded before
	for (unsigned int i = 0; i < scene->textures.size(); i++)
	{
		if (strncmp(image_path, scene->textures[i]->path, strlen(image_path)) == 0)
		{
			return scene->textures[i];
		}
	}
	return sgsdl2_create_texture(scene, image_path);
}

void sgsdl2_attach_texture_image(sgsdl2_texture * texture, const char *image_path)
{
	SDL_Surface *image = IMG_Load(image_path);
	if (!image)
	{
		sgsdl2_print_error(ERR_IMAGE_COULDNT_LOAD);
		return;
	}

	GLenum format = 0;
	if (image->format->Amask != 0)
	{
		// An alpha channel is present
		format = GL_RGBA;
	}
	else
	{
		format = GL_RGB;
	}

//	format = GL_RGBA;
//	bool alpha_present = image->format->Amask != 0;
//	unsigned int pixel_size = (alpha_present ? 4 : 3);
//	unsigned int count = image->h * image->w;
//	unsigned int current_pos = 0;
//	Uint8 *pixels = (Uint8*) malloc(sizeof(Uint8) * count * pixel_size);
//	Uint32 channel, pixel;
//
//	for (unsigned int i = 0; i < count; i++)
//	{
//		// This pixel color is a combination of RGB and A values.
//		// ie. #FF00FFFF
//		pixel = ((Uint32*)image->pixels)[current_pos];
//
//		/* Get Red component */
//		channel = pixel & format->Rmask;		/* Isolate red component */
//		channel = channel >> format->Rshift;	/* Shift it down to 8-bit */
//		channel = channel << format->Rloss;		/* Expand to a full 8-bit number */
//		pixels[current_pos + 0] = (Uint8) channel;
//
//		/* Get Green component */
//		channel = pixel & format->Gmask;		/* Isolate green component */
//		channel = channel >> format->Gshift;	/* Shift it down to 8-bit */
//		channel = channel << format->Gloss;		/* Expand to a full 8-bit number */
//		pixels[current_pos + 1] = (Uint8) channel;
//
//		/* Get Blue component */
//		channel = pixel & format->Bmask;		/* Isolate blue component */
//		channel = channel >> format->Bshift;	/* Shift it down to 8-bit */
//		channel = channel << format->Bloss;		/* Expand to a full 8-bit number */
//		pixels[current_pos + 2] = (Uint8) channel;
//
//		/* Get Alpha component */
//		channel = pixel & format->Amask;		/* Isolate alpha component */
//		channel = channel >> format->Ashift;	/* Shift it down to 8-bit */
//		channel = channel << format->Aloss;		/* Expand to a full 8-bit number */
//		pixels[current_pos + 3] = (Uint8) channel;
//
//		current_pos += pixel_size;
//	}

	format = GL_RGBA;


	// TODO this works for the textures tested, but according to
	// https://www.libsdl.org/release/SDL-1.2.15/docs/html/sdlpixelformat.html
	// the pixel data may need to be converted before being passed to OpenGL
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	// Internal format is how the texture is stored on the disk
	// Format defines the pixel data
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image->w, image->h, 0, format, GL_UNSIGNED_BYTE, image->pixels);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_image: ");
	
	// Assign a path to the texture object
	// TODO this should probably be moved into create_texture(scene, path)
	texture->path = (char*) malloc(sizeof(char) * strlen(image_path));
	memcpy(texture->path, image_path, sizeof(char) * strlen(image_path));

	SDL_FreeSurface(image);
}

void sgsdl2_change_texture_wrapping(sgsdl2_texture const * const texture, GLint const wrapping_s, GLint const wrapping_t)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	if (wrapping_s != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapping_s);
	}
	if (wrapping_t != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapping_t);
	}
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_wrapping: ");
}

void sgsdl2_change_texture_filtering(sgsdl2_texture const * const texture, GLint const min, GLint const mag)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	if (min != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min);
	}
	if (mag != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag);
	}
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_filtering: ");
}

void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const * const texture)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_image: ");
}

bool sgsdl2_is_texture(sgsdl2_texture *tex)
{
	return (tex && glIsTexture(tex->handle));
}

void sgsdl2_delete_texture(sgsdl2_texture *texture)
{
	glDeleteTextures(1, &texture->handle);
	delete [] texture->path;
	delete texture;
}
