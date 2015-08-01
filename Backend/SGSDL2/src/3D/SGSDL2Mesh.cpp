//
//  SGSDL2Mesh.cpp
//  sgsdl2
//
//  Created by James Ferguson on 30/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Mesh.h"
#include "SGSDL2Utilities.h"


sgsdl2_mesh *sgsdl2_create_mesh(sgsdl2_node *parent)
{
	// Node must belong to a scene
	if (!parent->root)
	{
		// TODO emit warning
		return nullptr;
	}
	
	if (parent->mesh)
	{
		// TODO emit warning
		return nullptr;
	}
	
	sgsdl2_mesh *result = new sgsdl2_mesh();
	result->parent = parent;
	parent->mesh = result;
	glGenVertexArrays(1, &result->vao);
	sgsdl2_check_opengl_error("create_mesh: ");
	return result;
}

void sgsdl2_add_attribute(sgsdl2_mesh *mesh, SGuint index, SGuint count, SGint size, int stride, SGfloat const *data)
{
	if (mesh->vao == 0)
	{
		// TODO emit warning
		return;
	}
	
	// Delete the previous action if needed.
	if (sgsdl2_possesses_attribute_at(mesh, index))
	{
		// TODO emit warning
		sgsdl2_delete_attribute_at(mesh, index);
	}
	
	sgsdl2_attribute attr = sgsdl2_attribute();
	attr.location = index;
	glGenBuffers(1, &attr.handle);
	glBindBuffer(GL_ARRAY_BUFFER, attr.handle);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(SGfloat) * count), data, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("add_attribute@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(mesh->vao);
	glEnableVertexAttribArray(index);
	glVertexAttribPointer(index, size, GL_FLOAT, false, stride, 0);
	// TODO check for error
	sgsdl2_check_opengl_error("add_attribute@data_format: ");
	
	mesh->attributes.push_back(attr);
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

bool sgsdl2_possesses_attribute_at(sgsdl2_mesh *mesh, SGuint index)
{
	for (unsigned long i = 0; i < mesh->attributes.size(); i++)
	{
		if (mesh->attributes[i].location == index)
		{
			return true;
		}
	}
	return false;
}

void sgsdl2_delete_attribute_at(sgsdl2_mesh *mesh, SGuint index)
{
	for (unsigned long i = 0; i < mesh->attributes.size(); i++)
	{
		if (mesh->attributes[i].location == index)
		{
			sgsdl2_attribute attr = mesh->attributes[i];
			glBindBuffer(GL_ARRAY_BUFFER, attr.handle);
			// Unbind it from the vao if needed
			if (mesh->vao != 0)
			{
				glBindVertexArray(mesh->vao);
				glDisableVertexAttribArray(index);
				glBindVertexArray(0);
			}
			
			glDeleteBuffers(1, &attr.handle);
			mesh->attributes.erase(mesh->attributes.begin() + (int) i);
		}
	}
}

SGuint sgsdl2_get_attribute_handle(sgsdl2_mesh *mesh, SGuint index)
{
	for (unsigned long i = 0; i < mesh->attributes.size(); i++)
	{
		if (mesh->attributes[i].location == index)
		{
			return mesh->attributes[i].handle;
		}
	}
	return 0;
}

void sgsdl2_attach_vertices(sgsdl2_mesh *mesh, SGfloat const *vertices, SGuint count)
{
	sgsdl2_add_attribute(mesh, MESH_ATTR_VERTICES, count, 3, 0, vertices);
}

void sgsdl2_attach_normals(sgsdl2_mesh *mesh, SGfloat const *normals, SGuint count)
{
	sgsdl2_add_attribute(mesh, MESH_ATTR_NORMALS, count, 3, 0, normals);
}

void sgsdl2_attach_texcoords(sgsdl2_mesh *mesh, SGfloat const *coords, SGuint count)
{
	sgsdl2_add_attribute(mesh, MESH_ATTR_TEXCOORDS, count, 2, 0, coords);
}

void sgsdl2_attach_indices(sgsdl2_mesh *mesh, SGushort const *indices, SGuint count)
{
	// Delete the previous buffer if needed
	if (mesh->indices_handle != 0)
	{
		glDeleteBuffers(1, &mesh->indices_handle);
		mesh->indices_count = 0;
	}
	
	glGenBuffers(1, &mesh->indices_handle);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indices_handle);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLushort) * count), indices, GL_STATIC_DRAW);
	mesh->indices_count = count;
	// TODO check for error
	sgsdl2_check_opengl_error("attach_indices: ");
	
	// Unbind buffers
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
}

bool sgsdl2_can_mesh_be_rendered(sgsdl2_mesh const *mesh)
{
	bool present_attrs[MESH_ATTR_LAST_RES];
	fill_n(present_attrs, MESH_ATTR_LAST_RES, false);
	
	for (unsigned long i = 0; i < mesh->attributes.size(); i++)
	{
		if (mesh->attributes[i].location < MESH_ATTR_LAST_RES)
		{
			present_attrs[i] = true;
		}
	}
	
	return (mesh->vao != 0
			&& mesh->indices_handle != 0
			&& present_attrs[MESH_ATTR_VERTICES]
			&& present_attrs[MESH_ATTR_NORMALS]);
}

SGuint sgsdl2_get_shader(sgsdl2_mesh const *mesh, sgsdl2_shader_mode mode)
{
	SGuint shader = 0;
	
	if (mode == sgsdl2_shader_mode::FULL)
	{
//		shader = mesh->material->shader;
//		if (mesh->material->shader > 0)
//		{
//			return mesh->material->shader;
//		}
//		else
//		{
//			shader = mesh->material->shader;
//		}
	}
	else
	{
		shader = mesh->material->depth_shader;
	}
	return shader;
}

void sgsdl2_delete_mesh(sgsdl2_mesh *mesh)
{
	// Delete each of the attributes
	for (unsigned long i = 0; i < mesh->attributes.size(); i++)
	{
		// TODO this is highly inefficient
		sgsdl2_delete_attribute_at(mesh, mesh->attributes[i].location);
	}
	
	// Delete the vao
	glDeleteVertexArrays(1, &mesh->vao);
}

