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

// Creates a sgsdl2_node to represent the source one and adds it as a child to dest
// Recursively creates child nodes as well.
void sgsdl2_recursively_create_node(sgsdl2_scene *scene, const aiScene *imported_scene, aiNode *source, sgsdl2_node *dest);

void sgsdl2_create_mesh(sgsdl2_node *node, const aiScene *imported_scene, aiMesh *mesh);

void sgsdl2_create_material(sgsdl2_mesh *mesh, aiMaterial *mat);

#endif /* defined(__sgsdl2__SGSDL2Importer__) */
