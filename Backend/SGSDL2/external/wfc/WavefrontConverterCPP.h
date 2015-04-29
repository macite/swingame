//
//  WavefrontConverter.h
//  WavefrontConverter
//
//  Created by James Ferguson on 23/04/2015.
//
//

#ifndef __WavefrontConverter__WavefrontConverter__
#define __WavefrontConverter__WavefrontConverter__

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cmath>
#include <iostream>
#include <fstream>
#include <vector>


// Contains all of the data assiciated with each file.
typedef struct {
	char *name;
	unsigned int name_length;
	
	float *vertices;
	unsigned int vertices_count;
	
	float *tex_coords;
	unsigned int tex_coords_count;
	
	float *normals;
	unsigned int normals_count;
	
	unsigned short *indices;
	unsigned int indices_count;
} obj_data;


// Reads all of the objects found in the .obj file and stores them in objects.
// count is the number of objects that were converted.
void convert_file(const char *file_name, obj_data **objects, int *count);

#endif /* defined(__WavefrontConverter__WavefrontConverter__) */
