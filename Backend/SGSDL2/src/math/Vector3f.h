//
//  Vector3f.h
//  GraphicsEngine
//
//  Created by James Ferguson on 28/01/13.
//  Copyright (c) 2013 James Ferguson. All rights reserved.
//

#ifndef __GraphicsEngine__Vector3f__
#define __GraphicsEngine__Vector3f__

#include <iostream>
#include <math.h>
//#include "Vector2f.h"


// Vector3f
// This may cause problems later as y is not guaranteed to be in the same location as v[1];
// These could be rewritten as std::array<float, 3>
typedef union _Vector3f {
	struct {
		float x, y, z;
	};
	float v[3];
} Vector3f;


// Prototypes
static inline Vector3f* makeVector3f(float x, float y, float z);
//static inline Vector3f* makeVector3f(Vector2f v, float z);
static inline Vector3f addVector3f(Vector3f v1, Vector3f v2);
static inline Vector3f negateVector3f(Vector3f v);
static inline Vector3f scalarMultiply(Vector3f v, float s);
static inline float dotProduct3f(Vector3f v1, Vector3f v2);
static inline Vector3f crossProduct3f(Vector3f v1, Vector3f v2);
static inline Vector3f normalize3f(Vector3f v);
static inline float magnitude(Vector3f v);



// Actual Functions
Vector3f* makeVector3f(float x, float y, float z)
{
	Vector3f *v = new Vector3f;
	v->x = x;
	v->y = y;
	v->z = z;
	return v;
}


//Vector3f* makeVector3f(Vector2f v, float z)
//{
//	Vector3f *r = new Vector3f;
//	r->x = v.x;
//	r->y = v.y;
//	r->z = z;
//	return r;
//}


Vector3f addVector3f(Vector3f v1, Vector3f v2)
{
	Vector3f r = { {
		v1.x + v2.x,
		v1.y + v2.y,
		v1.z + v2.z
			} };
	return r;
}


Vector3f negateVector3f(Vector3f v)
{
	Vector3f r = { { -v.x, -v.y, -v.z } };
	return r;
}


Vector3f scalarMultiply(Vector3f v, float s)
{
	Vector3f r = { {
		v.x * s,
		v.y * s,
		v.z * s
			} };
	return r;
}


float dotProduct3f(Vector3f v1, Vector3f v2)
{
	float result = (v1.x * v2.x
					+ v1.y * v2.y
					+ v1.z * v2.z);
	return result;
}


Vector3f crossProduct3f(Vector3f v1, Vector3f v2)
{
	Vector3f v = { { v1.y * v2.z - v1.z * v2.y,
		v1.z * v2.x - v1.x * v2.z,
		v1.x * v2.y - v1.y * v2.x } };
	return v;
}


Vector3f normalize3f(Vector3f v)
{
	float mag = sqrtf(v.x * v.x
					  + v.y * v.y
					  + v.z * v.z);
	if (mag == 0) mag = 1;

	Vector3f r = { {
		v.x / mag,
		v.y / mag,
		v.z / mag
			} };
	return r;
}


float magnitude(Vector3f v)
{
	return sqrtf(v.x * v.x + v.y * v.y + v.z * v.z);
}


#endif /* defined(__GraphicsEngine__Vector3f__) */
