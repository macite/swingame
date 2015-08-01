//
//  SGSDL2Mesh.h
//  sgsdl2
//
//  Created by James Ferguson on 30/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Mesh__
#define __sgsdl2__SGSDL2Mesh__

#include "SGSDL2Types.h"


// Creates an empty mesh for the given node.
sgsdl2_mesh* sgsdl2_create_mesh(sgsdl2_node *parent);

// Generates an attribute object and adds it to the mesh's array.
void sgsdl2_add_attribute(sgsdl2_mesh *mesh, SGuint index, SGuint count, SGint size, SGuint stride, SGfloat const *data);

// Returns true if the mesh has an attribute with the given index.
bool sgsdl2_possesses_attribute_at(sgsdl2_mesh *mesh, SGuint index);

// Deletes the buffer data off of the graphics card and then removes the attribute from the mesh's array.
void sgsdl2_delete_attribute_at(sgsdl2_mesh *mesh, SGuint index);

SGuint sgsdl2_get_attribute_handle(sgsdl2_mesh *mesh, SGuint index);

// Shortcut method to edit the vertices attribute
void sgsdl2_attach_vertices(sgsdl2_mesh *mesh, SGfloat const *vertices, SGuint count);

// Shortcut method to edit the normals attribute
void sgsdl2_attach_normals(sgsdl2_mesh *mesh, SGfloat const *normals, GLuint count);

// Shortcut method to edit the texcoords attribute
void sgsdl2_attach_texcoords(sgsdl2_mesh *mesh, SGfloat const *coords, GLuint count);

// Indices are handled differently to other buffers and therefore can only be set through this function.
void sgsdl2_attach_indices(sgsdl2_mesh *mesh, SGushort const *indices, SGuint count);

// Checks that the required buffers are non-zero
// TODO probably shouldn't be used
bool sgsdl2_can_mesh_be_rendered(sgsdl2_mesh const *mesh);

// Should be moved to material file
__attribute__((deprecated))
SGuint sgsdl2_get_shader(sgsdl2_mesh const *mesh, sgsdl2_shader_mode mode);

// Should be moved to material file
__attribute__((deprecated))
bool sgsdl2_valid_shader_available(sgsdl2_mesh const *mesh, sgsdl2_shader_mode mode);

void sgsdl2_delete_mesh(sgsdl2_mesh *mesh);


#endif /* defined(__sgsdl2__SGSDL2Mesh__) */
