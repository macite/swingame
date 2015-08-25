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
#include "SGSDL2Material.h"
#include <iostream>


void sgsdl2_populate_scene_from_file(sgsdl2_scene *scene, const char *filename)
{
	// Confirm the scene is valid
	if (!scene->root_node)
	{
		sgsdl2_print_error(ERR_SCENE_NO_ROOT);
		return;
	}
	
	const aiScene *imported_scene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_FlipUVs);
	if (!imported_scene)
	{
		sgsdl2_print_error(ERR_SCENE_COULDNT_IMPORT);
		cout << aiGetErrorString();
		aiReleaseImport(imported_scene);
		return;
	}
	
	sgsdl2_import_details details;
	details.root_path = sgsdl2_find_root_path(filename);
	details.dest = scene;
	details.source = imported_scene;
	details.material_indices = nullptr;
	
	// Load resources
	sgsdl2_load_materials(details);
//	sgsdl2_load_meshes(details);
	
	// Create all children of the root node
	for (unsigned int i = 0; i < imported_scene->mRootNode->mNumChildren; i++)
	{
//		sgsdl2_recursively_create_node(scene, imported_scene, imported_scene->mRootNode->mChildren[i], nullptr, filename);
		sgsdl2_convert_node(details, imported_scene->mRootNode->mChildren[i], nullptr);
	}
	
	aiReleaseImport(imported_scene);
	free(details.root_path);
	free(details.material_indices);
//	free(details.texture_indices);
}

//void sgsdl2_load_textures(sgsdl2_import_details det)
//{
//	if (det.source->HasTextures())
//	{
//		det.texture_indices = (sgsdl2_texture**) malloc(sizeof(sgsdl2_texture*) * det.source->mNumTextures);
//
//		for (unsigned int i = 0; i < det.source->mNumTextures; i++)
//		{
//			det.texture_indices[i] = sgsdl2_create_texture(det.dest, det.source->mTextures[i]->)
//		}
//	}
//	else
//	{
//		det.texture_indices = nullptr;
//	}
//}

void sgsdl2_load_materials(sgsdl2_import_details &det)
{
	det.material_indices = (sgsdl2_material**) malloc(sizeof(sgsdl2_material*) * det.source->mNumMaterials);
	if (det.source->HasMaterials())
	{
		for (unsigned int i = 0; i < det.source->mNumMaterials; i++)
		{
			det.material_indices[i] = sgsdl2_convert_material(det, det.source->mMaterials[i]);
		}
	}
	else
	{
		fill_n(&det.material_indices[0], det.source->mNumMaterials, nullptr);
	}
}

void sgsdl2_load_meshes(sgsdl2_import_details &det)
{
#pragma unused(det)
//	det.mesh_indices = (sgsdl2_mesh**) malloc(sizeof(sgsdl2_mesh*) * det.source->mNumMeshes);
//	if (det.source->HasMeshes())
//	{
//		for (unsigned int i = 0; i < det.source->mNumMeshes; i++)
//		{
//			det.mesh_indices[i] = sgsdl2_convert_mesh(det, det.source->mMeshes[i]);
//		}
//	}
//	else
//	{
//		fill_n(&det.mesh_indices[0], det.source->mNumMeshes, nullptr);
//	}
}

sgsdl2_node* sgsdl2_convert_node(const sgsdl2_import_details det, aiNode *source_node, sgsdl2_node *parent)
{
	if (parent == nullptr)
	{
		parent = det.dest->root_node;
	}
	
	sgsdl2_node *new_node = sgsdl2_create_new_node(parent);
	
	// Recursively add children first to save memory
	for (unsigned int i = 0; i < source_node->mNumChildren; i++)
	{
		sgsdl2_convert_node(det, source_node->mChildren[i], new_node);
	}
	
	aiVector3D location;
	aiVector3D scale;
	aiQuaternion rotation;
	source_node->mTransformation.Decompose(scale, rotation, location);
	new_node->location = sgsdl2_vec3(location);
	new_node->scale = sgsdl2_vec3(scale);
	new_node->rotation = sgsdl2_angles_from_quat(sgsdl2_vec4(rotation));
	
	mat4 trans = sgsdl2_get_local_transform(new_node);
	bool transforms_match = true;
	for (unsigned int i = 0; i < 16; i++)
	{
		if (value_ptr(trans)[i] != *source_node->mTransformation[i])
		{
			transforms_match = false;
			break;
		}
	}
	if (!transforms_match)
	{
		// TODO emit warning
		// Not sure if there would be an accurate way to detect meaning differences here, as the transform may be slightly different due to the conversion between euler angle rotation and quats.
	}
	
	// Load meshes
	// Only one mesh
	if (source_node->mNumMeshes == 1)
	{
//		new_node->mesh = det.dest->meshes[source_node->mMeshes[0]];
		sgsdl2_convert_mesh(det, det.source->mMeshes[source_node->mMeshes[0]], new_node);
	}
	// Multiple meshes
	else if (source_node->mNumMeshes > 1)
	{
		for (unsigned int i = 0; i < source_node->mNumMeshes; i++)
		{
			// Create an empty sub node for each mesh
			sgsdl2_node *sub_node = sgsdl2_create_new_node(new_node);
//			sub_node->mesh = det.dest->meshes[source_node->mMeshes[i]];
			sgsdl2_convert_mesh(det, det.source->mMeshes[source_node->mMeshes[i]], sub_node);
		}
	}
	
	return new_node;
}

sgsdl2_mesh* sgsdl2_convert_mesh(const sgsdl2_import_details det, aiMesh *source_mesh, sgsdl2_node *parent)
{
	sgsdl2_mesh *new_mesh = sgsdl2_create_mesh(parent);
	
	float *data_array = nullptr;
	unsigned int size = 0;
	
	sgsdl2_flatten_array(source_mesh->mVertices, source_mesh->mNumVertices, data_array, size);
	sgsdl2_attach_vertices(new_mesh, data_array, size);
	// Index list is not supported when importing
	new_mesh->indices_count = size;
	
	if (source_mesh->HasNormals())
	{
		sgsdl2_flatten_array(source_mesh->mNormals, source_mesh->mNumVertices, data_array, size);
		sgsdl2_attach_normals(new_mesh, data_array, (GLuint) size);
	}
	if (source_mesh->HasTextureCoords(0))
	{
		sgsdl2_flatten_array(source_mesh->mTextureCoords[0], source_mesh->mNumVertices, data_array, size, 2); // UVs are only supported in 2 dimensions.
		sgsdl2_attach_texcoords(new_mesh, data_array, size);
	}
	
	// Attach material
	new_mesh->material = det.material_indices[source_mesh->mMaterialIndex];
	return new_mesh;
}

sgsdl2_material* sgsdl2_convert_material(const sgsdl2_import_details det, aiMaterial *source_mat)
{
	sgsdl2_material *new_mat = sgsdl2_create_material(det.dest);
	det.dest->materials.push_back(new_mat);
	
	// Places to store properties
	aiColor4D color4D_param(0.f, 0.f, 0.f, 0.f);
	aiString string_param("");
	float float_param = 0;
	
	// Ambient color
	if (AI_SUCCESS == aiGetMaterialColor(source_mat, AI_MATKEY_COLOR_AMBIENT, &color4D_param))
	{
		// TODO implement ambient color
		//		new_mat.ambient_color = sgsdl2_color(color4D);
	}
	
	// Diffuse color
	if (AI_SUCCESS == aiGetMaterialColor(source_mat, AI_MATKEY_COLOR_DIFFUSE, &color4D_param))
	{
		new_mat->diffuse_color = sgsdl2_color(color4D_param);
	}
	
	// Specular color
	if (AI_SUCCESS == aiGetMaterialColor(source_mat, AI_MATKEY_COLOR_SPECULAR, &color4D_param))
	{
		new_mat->specular_color = sgsdl2_color(color4D_param);
	}
	
	// Specular exponent
	if (AI_SUCCESS == aiGetMaterialFloat(source_mat, AI_MATKEY_SHININESS, &float_param))
	{
		new_mat->specular_exponent = float_param;
		float_param = 0;
	}
	
	// Specular intensity
	if (AI_SUCCESS == aiGetMaterialFloat(source_mat, AI_MATKEY_SHININESS_STRENGTH, &float_param))
	{
		new_mat->specular_intensity = float_param;
		float_param = 0;
	}
	
	// Textures (Swingame only accepts the first texture of each stack)
	if (AI_SUCCESS == aiGetMaterialTexture(source_mat, aiTextureType_DIFFUSE, 0, &string_param))
	{
		new_mat->diffuse_texture = sgsdl2_convert_texture(det, string_param.C_Str());
		string_param.Clear();
	}
	
	if (AI_SUCCESS == aiGetMaterialTexture(source_mat, aiTextureType_SPECULAR, 0, &string_param))
	{
		new_mat->specular_texture = sgsdl2_convert_texture(det, string_param.C_Str());
		string_param.Clear();
	}
	
	if (AI_SUCCESS == aiGetMaterialTexture(source_mat, aiTextureType_NORMALS, 0, &string_param))
	{
		new_mat->normal_map = sgsdl2_convert_texture(det, string_param.C_Str());
		string_param.Clear();
	}
	
	return new_mat;
}

sgsdl2_texture *sgsdl2_convert_texture(const sgsdl2_import_details det, const char *rel_path)
{
	// Convert the image path to relative to the scene.
	unsigned long full_path_length = strlen(rel_path) + strlen(det.root_path) + 1;
	char *full_path = (char*) malloc(sizeof(char) * full_path_length);
	// Clear the string
	for (int i = 0; i < full_path_length; i++) full_path[i] = '\0';
	strlcat(full_path, det.root_path, 512);
	strlcat(full_path, rel_path, 512);
	full_path[full_path_length - 1] = '\0';
	
	// Load relatively
	sgsdl2_texture *tex = sgsdl2_create_texture(det.dest, full_path);
	free(full_path);
	
	if (!tex)
	{
		sgsdl2_print_error(ERR_IMAGE_IMPORT_FAILED);
		return nullptr;
	}
	
	// Default assign filtering and wrapping
	sgsdl2_change_texture_wrapping(tex, GL_REPEAT, GL_REPEAT);
	sgsdl2_change_texture_filtering(tex, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
	sgsdl2_generate_texture_mipmaps(tex);
	return tex;
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
	
//	// Textures (Swingame only accepts the first texture of each stack)
//	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_DIFFUSE, 0, &string_param))
//	{
//		new_mat->diffuse_texture = sgsdl2_import_texture(string_param, filename)->handle;
//	}
//	
//	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_SPECULAR, 0, &string_param))
//	{
//		new_mat->specular_texture = sgsdl2_import_texture(string_param, filename)->handle;
//	}
//	
//	if (AI_SUCCESS == aiGetMaterialTexture(mat, aiTextureType_NORMALS, 0, &string_param))
//	{
//		new_mat->normal_map = sgsdl2_import_texture(string_param, filename)->handle;
//	}
	
	mesh->material = new_mat;
}

//sgsdl2_texture* sgsdl2_import_texture(aiString relative_image_path, const char* scene_filename)
//{
//	// Convert the image path to relative to the scene.
//	string scene_path(scene_filename);
//	scene_path.erase(scene_path.find_last_of(FILENAME_SEPARATOR) + 1, string::npos);
//	string full_image_path = scene_path + string(relative_image_path.C_Str());
//	
//	// Load relatively
//	sgsdl2_texture *tex = sgsdl2_create_texture(full_image_path.c_str());
//	if (!tex)
//	{
//		sgsdl2_print_error(ERR_IMAGE_IMPORT_FAILED);
//		return nullptr;
//	}
//	
//	// Default assign filtering and wrapping
//	sgsdl2_change_texture_wrapping(tex, GL_REPEAT, GL_REPEAT);
//	sgsdl2_change_texture_filtering(tex, GL_LINEAR, GL_LINEAR);
//	
//	return tex;
//}

char* sgsdl2_find_root_path(const char *filename)
{
	string scene_path(filename);
	scene_path.erase(scene_path.find_last_of(FILENAME_SEPARATOR) + 1, string::npos);
	size_t string_size = sizeof(char) * scene_path.length();
	char* return_string = (char*) malloc(string_size);
	strncpy(return_string, scene_path.c_str(), string_size);
	return return_string;
}



