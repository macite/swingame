//
//  SGSDL2Renderer.h
//  sgsdl2
//
//  Created by James Ferguson on 16/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Renderer__
#define __sgsdl2__SGSDL2Renderer__

#include "SGSDL2Types.h"


// PRIVATE
// Rerenders shadow maps for lights that need it
void sgsdl2_rerender_shadow_maps(sgsdl2_scene *scene, sgsdl2_renderer *renderer);

// Performs a render pass of the scene
void sgsdl2_perform_render_pass(sgsdl2_renderer *renderer, sgsdl2_camera *camera, sgsdl2_render_options opts);

// PRIVATE
// Calls render_node on the given node, and then iterates over all its children.
void sgsdl2_render_nodes(sgsdl2_node *node, sgsdl2_renderer *renderer);

// PRIVATE
// Renders a single node and its elements
void sgsdl2_render_node(sgsdl2_node *node, sgsdl2_renderer *renderer);

// PRIVATE
// Renders a single mesh
void sgsdl2_render_mesh(sgsdl2_mesh *mesh, sgsdl2_renderer *renderer);

// PRIVATE
void sgsdl2_calculate_lighting_state(sgsdl2_scene *scene, sgsdl2_renderer *renderer);

// PRIVATE
void sgsdl2_calculate_camera_state(sgsdl2_camera *camera, sgsdl2_renderer *renderer);

// PRIVATE
void sgsdl2_calculate_node_state(sgsdl2_node *node, sgsdl2_renderer *renderer);

// PRIVATE
void sgsdl2_calculate_material_state(sgsdl2_material *mat, sgsdl2_renderer *renderer);

// PRIVATE
// Calculates the mvp matrix.
void sgsdl2_complete_shader_interface(sgsdl2_shader_interface &interface);





#endif /* defined(__sgsdl2__SGSDL2Renderer__) */
