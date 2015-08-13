//
//  SGSDL2Material.cpp
//  sgsdl2
//
//  Created by James Ferguson on 7/08/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Material.h"


sgsdl2_material* sgsdl2_create_material(sgsdl2_scene *scene)
{
	sgsdl2_material *mat = new sgsdl2_material();
	scene->materials.push_back(mat);
	return mat;
}


void sgsdl2_delete_material(sgsdl2_material *material)
{
	delete material;
}


