//
//  SGSDL2Node.h
//  sgsdl2
//
//  Created by James Ferguson on 28/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Node__
#define __sgsdl2__SGSDL2Node__

#include <stdio.h>
#include "SGSDL2Types.h"


// Creates a new node and adds it to the root node of the scene
sgsdl2_node* sgsdl2_create_new_node(sgsdl2_scene *scene);

// Creates a new node with the given node as a parent
sgsdl2_node* sgsdl2_create_new_node(sgsdl2_node *parent);

// Creates a new node at the given location with the given node as a parent.
sgsdl2_node* sgsdl2_create_new_node(sgsdl2_node *parent, vec3 location, vec3 rotation, vec3 scale);

// PRIVATE
// Creates the root node for the scene.
void sgsdl2_create_root_node(sgsdl2_scene *scene);

// Note: this is an overload of a function in Scene.h
void sgsdl2_add_node(sgsdl2_node *parent, sgsdl2_node *child);

// Recursively removes all children from this node, then removes this node from its parent.
void sgsdl2_remove_node(sgsdl2_node *node);

// Removes all children from this node.
void sgsdl2_remove_all_children(sgsdl2_node *node);

// Moves a node from one parent to another without effecting its children.
void sgsdl2_move_node(sgsdl2_node *child, sgsdl2_node *new_parent);

// Removes a node from its parent without effecting its children.
// PRIVATE
void sgsdl2_remove_node_from_parent(sgsdl2_node *node);

mat4 sgsdl2_get_local_transform(sgsdl2_node *node);

mat4 sgsdl2_get_global_transform(sgsdl2_node *node);

mat4 sgsdl2_get_view_transform(sgsdl2_node *node);

vec3 sgsdl2_get_global_location(sgsdl2_node *node);

void sgsdl2_delete_node(sgsdl2_node *node);


#endif /* defined(__sgsdl2__SGSDL2Node__) */
