//
//  SGSDL2Importer.h
//  sgsdl2
//
//  Created by James Ferguson on 24/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Importer__
#define __sgsdl2__SGSDL2Importer__

#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include "SGSDL2Types.h"


// Adds the geometry from the file to the scene
void sgsdl2_populate_scene_from_file(sgsdl2_scene *scene, const char* filename);

// Converts all the aiTexture objects into sgsdl2_texture objects.
// Also populates the texture_indices array of import_details.
//void sgsdl2_load_textures(sgsdl2_import_details det);

// Converts all the aiMaterial objects into sgsdl2_material objects.
// Allocates the material_indices array in import_details
void sgsdl2_load_materials(sgsdl2_import_details &det);

__attribute__((deprecated))
void sgsdl2_load_meshes(sgsdl2_import_details &det);

// Creates a sgsdl2_node to represent the source one and adds it as a child to dest
// Recursively creates child nodes as well.
__attribute__((deprecated))
void sgsdl2_recursively_create_node(sgsdl2_scene *scene, const aiScene *imported_scene, aiNode *source, sgsdl2_node *dest, const char *filename);

__attribute__((deprecated))
void sgsdl2_create_mesh(sgsdl2_node *node, const aiScene *imported_scene, aiMesh *mesh, const char *filename);

__attribute__((deprecated))
void sgsdl2_create_material(sgsdl2_mesh *mesh, aiMaterial *mat, const char *filename);

// Recursive.
// If parent is null, will add it to the root node of the dest.
// This function must be called with a parent node because nodes cannot exist by themselves.
sgsdl2_node* sgsdl2_convert_node(const sgsdl2_import_details det, aiNode *source_node, sgsdl2_node *parent);

// Creates a new mesh in the scene that matches the source_mesh.
// This function must be called with a parent node because meshes cannot exist by themselves.
sgsdl2_mesh* sgsdl2_convert_mesh(const sgsdl2_import_details det, aiMesh *source_mesh, sgsdl2_node *parent);

// Creates a new material in the scene that matches the source material.
sgsdl2_material* sgsdl2_convert_material(const sgsdl2_import_details det, aiMaterial *source_material);

// Creates a new texture in the scene that matches the source texture.
sgsdl2_texture* sgsdl2_convert_texture(const sgsdl2_import_details det, const char *rel_path);

__attribute__((deprecated))
sgsdl2_texture* sgsdl2_import_texture(aiString image_path, const char* filename);

// Returns a string that represents the folder the scene file is stored in.
// Used to allow other resources to be loaded relatively.
char* sgsdl2_find_root_path(const char *filename);

#endif /* defined(__sgsdl2__SGSDL2Importer__) */
