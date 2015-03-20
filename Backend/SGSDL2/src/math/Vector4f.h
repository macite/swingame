//
//  Vector4f.h
//  GraphicsEngine
//
//  Created by James Ferguson on 07/05/14.
//  Copyright (c) 2013 James Ferguson. All rights reserved.
//

#ifndef __GraphicsEngine__Vector4f__
#define __GraphicsEngine__Vector4f__

#include <iostream>
#include <math.h>


// Vector4f
typedef union {
	struct {
		float x, y, z, w;
	};
	float v[4];
} Vector4f;


// Prototypes
static inline Vector4f makeVector4f(float x, float y, float z, float w);
static inline Vector4f makeVector4f(Vector3f v, float z);
static inline Vector4f addVector4f(Vector4f v1, Vector4f v2);
static inline Vector4f negateVector4f(Vector4f v);
static inline Vector4f scalarMultiply(Vector4f v, float s);
static inline float dotProduct4f(Vector4f v1, Vector4f v2);
static inline Vector4f normalize4f(Vector4f v);

static inline std::string makeStringFromVector4f(Vector4f v);




// Actual Functions
Vector4f makeVector4f(float x, float y, float z, float w)
{
	Vector4f v = { { x, y, z, w } };
	return v;
}


Vector4f makeVector4f(Vector3f v, float w)
{
	Vector4f r = { { v.x, v.y, v.z, w } };
	return r;
}


Vector4f addVector4f(Vector4f v1, Vector4f v2)
{
	Vector4f r = { {
		v1.x + v2.x,
		v1.y + v2.y,
		v1.z + v2.z,
		v1.w + v2.w
			} };
	return r;
}


Vector4f negateVector4f(Vector4f v)
{
	Vector4f r = { { -v.x, -v.y, -v.z, -v.w } };
	return r;
}


Vector4f scalarMultiply(Vector4f v, float s)
{
	Vector4f r = { {
		v.x * s,
		v.y * s,
		v.z * s,
		v.w * s
			} };
	return r;
}


float dotProduct4f(Vector4f v1, Vector4f v2)
{
	float result = (v1.x * v2.x
					+ v1.y * v2.y
					+ v1.z * v2.z
					+ v1.w * v2.w);
	return result;
}


Vector4f normalize4f(Vector4f v)
{
	float mag = sqrtf(v.x * v.x
					  + v.y * v.y
					  + v.z * v.z
					  + v.w * v.w);
	Vector4f r = { { v.x / mag,
		v.y / mag,
		v.z / mag,
		v.w / mag
			} };
	return r;
}


std::string makeStringFromVector4f(Vector4f v)
{
	std::string s = "";
	s += "[ ";
	for (int i = 0; i < 4; i++)
	{
		s += std::to_string(v.v[i]) + ", ";
	}
	s += "]";
	return s;
}

#endif /* defined(__GraphicsEngine__Vector4f__) */
