//
//  SGSDL2Texture.h
//  sgsdl2
//
//  Created by James Ferguson on 30/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Texture__
#define __sgsdl2__SGSDL2Texture__

#include "SGSDL2Types.h"

// Creates an empty texture
sgsdl2_texture* sgsdl2_create_texture(sgsdl2_scene *scene);

// Creates an empty texture and automatically loads the given image into it
sgsdl2_texture* sgsdl2_create_texture(sgsdl2_scene *scene, const char *image_path);

sgsdl2_texture* sgsdl2_find_or_create_texture(sgsdl2_scene *scene, const char *image_path);

// Attaches an image to an existing texture
void sgsdl2_attach_texture_image(sgsdl2_texture * texture, const char * image_path);

// Changes the wrapping parameters for a texture
// The value of -1 means don't change that value
void sgsdl2_change_texture_wrapping(sgsdl2_texture const * const texture, SGint const wrapping_s = -1, SGint const wrapping_t = -1);

// Changes the filtering for a texture
// Min is when the image is too small, mag is when the image is too big ???
// The value of -1 means don't change that value
void sgsdl2_change_texture_filtering(sgsdl2_texture const * texture, SGint const min = -1, SGint const mag = -1);

// Generates texture mipmaps
void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const * texture);

// Returns true if the struct refers to a valid ogl texture
// Returns false if tex is nullptr
bool sgsdl2_is_texture(sgsdl2_texture *tex);

// Deletes the texture from opengl then deletes the pointer
void sgsdl2_delete_texture(sgsdl2_texture *texture);

#endif /* defined(__sgsdl2__SGSDL2Texture__) */
