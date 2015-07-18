//
//  SGSDL2Scene.h
//  sgsdl2
//
//  Created by James Ferguson on 29/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Scene__
#define __sgsdl2__SGSDL2Scene__

#include <stdio.h>
#include <vector>
#include "SGSDL2Types.h"

using namespace std;
using namespace glm;


// Creates an empty scene
sgsdl2_scene* sgsdl2_make_scene(sg_drawing_surface *surface);

// PRIVATE
// Compiles the default shaders that are used behind the scenes
void sgsdl2_compile_default_shaders(sgsdl2_scene *scene);

// PRIVATE
// Creates an array texture for the lights in the scene to use.
// The number of layers is the same as the maximum number of lights.
void sgsdl2_create_default_array_texture(sgsdl2_scene *scene);

// Adds a node to the scene. This is just a convenience method.
// Note: this is an overload of a function in Node.h
void sgsdl2_add_node(sgsdl2_scene *scene, sgsdl2_node *node);

// Simply calls remove_element() on all elements in the root set.
void sgsdl2_clear_scene(sgsdl2_scene *scene);

// Makes the camera the active one for the scene
// Triggers a reindex on next render.
void sgsdl2_set_active_camera(sgsdl2_scene *scene, sgsdl2_camera *cam);

// The single entry point for external render calls (from the user).
// Performs a full render of the scene.
void sgsdl2_render_scene(sgsdl2_scene *scene);

// Regenerates the lighting state.
void sgsdl2_rebuild_lighting(sgsdl2_scene *scene, sgsdl2_renderer *renderer);

// Recursive (depth first, top-down) iteration over the scene calling the given function on each node.
void sgsdl2_iterate_node(sgsdl2_node *node, void (*func)(sgsdl2_node*));

// Cleans up all the resources used by a scene and then deletes the pointer
void sgsdl2_delete_scene(sgsdl2_scene *scene);




#endif /* defined(__sgsdl2__SGSDL2Scene__) */
