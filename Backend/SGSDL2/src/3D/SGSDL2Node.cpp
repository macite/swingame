//
//  SGSDL2Node.cpp
//  sgsdl2
//
//  Created by James Ferguson on 28/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Node.h"
#include "SGSDL2Utilities.h"
#include <vector>


sgsdl2_node* sgsdl2_create_new_node(sgsdl2_scene *scene)
{
	if (!scene->root_node)
	{
		sgsdl2_print_error(ERR_SCENE_NO_ROOT);
		return nullptr;
	}
	
	return sgsdl2_create_new_node(scene->root_node);
}

sgsdl2_node* sgsdl2_create_new_node(sgsdl2_node *parent)
{
	// Node must belong to a scene
	if (!parent->root)
	{
		sgsdl2_print_error(ERR_NODE_NO_ROOT);
		return nullptr;
	}
	
	sgsdl2_node *result = new sgsdl2_node();
	result->parent = parent;
	result->root = parent->root;
	parent->children.push_back(result);
	return result;
}

sgsdl2_node* sgsdl2_create_new_node(sgsdl2_node *parent, vec3 location, vec3 rotation, vec3 scale)
{
	sgsdl2_node *result = sgsdl2_create_new_node(parent);
	result->location = location;
	result->rotation = rotation;
	result->scale = scale;
	return result;
}

void sgsdl2_create_root_node(sgsdl2_scene *scene)
{
	scene->root_node = new sgsdl2_node();
	scene->root_node->root = scene;
}

void sgsdl2_add_node(sgsdl2_node *parent, sgsdl2_node *child)
{
	// Nodes can only possess children if they belong to a scene.
	if (!parent->root)
	{
		sgsdl2_print_error(ERR_NODE_NO_ROOT);
		return;
	}
	
	// Remove the child from previous parent
	if (child->parent)
	{
		sgsdl2_remove_node(child);
	}
	parent->children.push_back(child);
	child->parent = parent;
	child->root = parent->root;
}

void sgsdl2_remove_node(sgsdl2_node *node)
{
	// The node can only be removed if it has a parent
	if (!node->parent)
	{
		sgsdl2_print_error(ERR_NODE_NO_PARENT);
		return;
	}
	
	// If the node has children, they are removed first
	sgsdl2_remove_all_children(node);
	
	// Then remove this node.
	sgsdl2_remove_node_from_parent(node);

	// TODO emit warning - child was not included in parent array.
}

void sgsdl2_remove_all_children(sgsdl2_node *node)
{
	for (unsigned long i = 0; i < node->children.size(); i++)
	{
		sgsdl2_remove_node(node->children[i]);
	}
}

void sgsdl2_move_node(sgsdl2_node *child, sgsdl2_node *new_parent)
{
	// Remove the node from its old parent
	if (child->parent)
	{
		sgsdl2_remove_node_from_parent(child);
	}
	
	// Add node to its new parent
	sgsdl2_add_node(new_parent, child);
}

void sgsdl2_remove_node_from_parent(sgsdl2_node *node)
{
	for (unsigned long i = 0; i < node->parent->children.size(); i++)
	{
		if (node == node->parent->children[i])
		{
			node->parent->children.erase(node->parent->children.begin() + (long) i);
			node->parent = nullptr;
			node->root = nullptr;
			return;
		}
	}
}

mat4 sgsdl2_get_local_transform(sgsdl2_node *node)
{
	// Following the format of FBX transforms
	mat4 trans = translate(mat4(1.0f), node->location);
	mat4 rot = eulerAngleXYZ(node->rotation.x, node->rotation.y, node->rotation.z);
	mat4 sca = scale(mat4(1.0f), node->scale);
	mat4 transform = mat4(1.0f) * trans * rot * sca;
	return transform;
}

mat4 sgsdl2_get_global_transform(sgsdl2_node *node)
{
	mat4 transform = sgsdl2_get_local_transform(node);
	if (node->parent)
	{
		transform = sgsdl2_get_global_transform(node->parent) * transform;
	}
	return transform;
}

mat4 sgsdl2_get_view_transform(sgsdl2_node *node)
{
	return inverse(sgsdl2_get_global_transform(node));
}

vec3 sgsdl2_get_global_location(sgsdl2_node *node)
{
	return vec3(sgsdl2_get_global_transform(node) * vec4(0, 0, 0, 1));
}

void sgsdl2_delete_node(sgsdl2_node *node)
{
#pragma unused(node)
	// TODO
}












