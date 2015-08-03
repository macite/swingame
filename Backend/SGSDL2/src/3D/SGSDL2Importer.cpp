//
//  SGSDL2Importer.cpp
//  sgsdl2
//
//  Created by James Ferguson on 24/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Importer.h"
#include "SGSDL2Node.h"
#include "SGSDL2Utilities.h"
#include "SGSDL2Mesh.h"
#include "SGSDL2Texture.h"
#include <iostream>


void sgsdl2_populate_scene_from_file(sgsdl2_scene *scene, const char *filename)
{
	// Confirm the scene is valid
	if (!scene->root_node)
	{
		// TODO emit warning
		return;
	}
	
	const aiScene *imported_scene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_FlipUVs);
	if (!imported_scene)
	{
		// TODO emit error
		cout << aiGetErrorString();
		return;
	}
	
	// Create all children of the root node
	for (unsigned int i = 0; i < imported_scene->mRootNode->mNumChildren; i++)
	{
		sgsdl2_recursively_create_node(scene, imported_scene, imported_scene->mRootNode->mChildren[i], nullptr, filename);
	}
	
	aiReleaseImport(imported_scene);
}

void sgsdl2_recursively_create_node(sgsdl2_scene *scene, const aiScene *imported_scene, aiNode *source, sgsdl2_node *dest, const char *filename)
{
	if (dest == nullptr)
	{
		dest = scene->root_node;
	}
	
	sgsdl2_node *new_node = sgsdl2_create_new_node(dest);
	
	// Recursively add children first to save memory
	// For some reason I am optimising for this
	for (unsigned int i = 0; i < source->mNumChildren; i++)
	{
		sgsdl2_recursively_create_node(scene, imported_scene, source->mChildren[i], new_node, filename);
	}
	
	aiVector3D location;
	aiVector3D scale;
	aiQuaternion rotation;
	source->mTransformation.Decompose(scale, rotation, location);
	new_node->location = sgsdl2_vec3(location);
	new_node->scale = sgsdl2_vec3(scale);
	new_node->rotation = sgsdl2_angles_from_quat(sgsdl2_vec4(rotation));
	
	mat4 trans = sgsdl2_get_local_transform(new_node);
	bool transforms_match = true;
	for (unsigned int i = 0; i < 16; i++)
	{
		if (value_ptr(trans)[i] != *source->mTransformation[i])
		{
			transforms_match = false;
			break;
		}
	}
	if (!transforms_match)
	{
		// TODO emit warning
	}
	
	// Only one mesh
	if (source->mNumMeshes == 1)
	{
		sgsdl2_create_mesh(new_node, imported_scene, imported_scene->mMeshes[source->mMeshes[0]], filename);
	}
	// Multiple meshes
	else if (source->mNumMeshes > 1)
	{
		for (unsigned int i = 0; i < source->mNumMeshes; i++)
		{
			// Create an empty sub node for each mesh
			sgsdl2_node *sub_node = sgsdl2_create_new_node(new_node);
			sgsdl2_create_mesh(sub_node, imported_scene, imported_scene->mMeshes[source->mMeshes[i]], filename);
		}
	}
}

void sgsdl2_create_mesh(sgsdl2_node *node, const aiScene *imported_scene, aiMesh *mesh, const char *filename)
{
	sgsdl2_mesh *new_mesh = sgsdl2_create_mesh(node);
	
	float *data_array = nullptr;
	unsigned int size = 0;
	
	sgsdl2_flatten_array(mesh->mVertices, mesh->mNumVertices, data_array, size);
	sgsdl2_attach_vertices(new_mesh, data_array, size);
	// Index list is not supported when importing
	new_mesh->indices_count = size;
	
	if (mesh->HasNormals())
	{
		sgsdl2_flatten_array(mesh->mNormals, mesh->mNumVertices, data_array, size);
		sgsdl2_attach_normals(new_mesh, data_array, (GLuint) size);
	}
	if (mesh->HasTextureCoords(0))
	{
		sgsdl2_flatten_array(mesh->mTextureCoords[0], mesh->mNumVertices, data_array, size, 2); // UVs are only supported in 2 dimensions.
		sgsdl2_attach_texcoords(new_mesh, data_array, size);
	}
	
	sgsdl2_create_material(new_mesh, imported_scene->mMaterials[mesh->mMaterialIndex], filename);
}

void sgsdl2_create_material(sgsdl2_mesh *mesh, aiMaterial *mat, const char *filename)
{
	sgsdl2_material *new_mat = new sgsdl2_material();
	
	// Places to store properties
	aiColor4D color4D_param(0.f, 0.f, 0.f, 0.f);
	aiString string_param("");
	float float_param = 0;
	
	// Ambient color
	if (AI_SUCCESS == aiGetMaterialColor(mat, AI_MATKEY_COLOR_AMBIENT, &color4D_param))
	{
		// TODO implement ambient color
//		new_mat.ambient_color = sgsdl2_color(color4D);
	}
	
	// Diffuse color
	if (AI_SUCCESS == aiGetMaterialColor(mat, AI_MATKEY_COLOR_DIFFUSE, &color4D_param))
	{
		new_mat->diffuse_color = sgsdl2_color(color4D_param);
	}
	
	// Specular color
	if (AI_SUCCESS == aiGetMaterialColor(mat, AI_MATKEY_COLOR_SPECULAR, &color4D_param))
	{
		new_mat->specular_color = sgsdl2_color(color4D_param);
	}
	
	// Specular exponent
	if (AI_SUCCESS == aiGetMaterialFloat(mat, AI_MATKEY_SHININESS, &float_param))
	{
		new_mat->specular_exponent = float_param;
	}
	
	// Specular intensity
	if (AI_SUCCESS == aiGetMaterialFloat(mat, AI_MATKEY_SHININESS_STRENGTH, &float_param))
	{
		new_mat->specular_intensity = float_param;
	}
	
	// Textures (Swingame only accepts the first texture of each stack)
	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_DIFFUSE, 0, &string_param))
	{
		new_mat->diffuse_texture = sgsdl2_import_texture(string_param, filename)->handle;
	}
	
	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_SPECULAR, 0, &string_param))
	{
		new_mat->specular_texture = sgsdl2_import_texture(string_param, filename)->handle;
	}
	
	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_NORMALS, 0, &string_param))
	{
		new_mat->normal_map = sgsdl2_import_texture(string_param, filename)->handle;
	}
	
	mesh->material = new_mat;
}

sgsdl2_texture* sgsdl2_import_texture(aiString relative_image_path, const char* scene_filename)
{
	// Convert the image path to relative to the scene.
	string scene_path(scene_filename);
	scene_path.erase(scene_path.find_last_of(FILENAME_SEPARATOR) + 1, string::npos);
	string full_image_path = scene_path + string(relative_image_path.C_Str());
	
	// Load relatively
	sgsdl2_texture *tex = sgsdl2_create_texture(full_image_path.c_str());
	if (!tex)
	{
		// TODO emit warning
		return nullptr;
	}
	
	// Default assign filtering and wrapping
	sgsdl2_change_texture_wrapping(tex, GL_REPEAT, GL_REPEAT);
	sgsdl2_change_texture_filtering(tex, GL_LINEAR, GL_LINEAR);
	
	return tex;
}



