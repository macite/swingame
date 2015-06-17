//
//  Matrix4f.h
//  GraphicsEngine v3
//
//  Created by James Ferguson on 13/04/2014.
//  Copyright (c) 2014 James Ferguson. All rights reserved.
//

#ifndef __GraphicsEngine_v3__Matrix4f__
#define __GraphicsEngine_v3__Matrix4f__

#include <iostream>
#include <math.h>
#include <string>
#include "Vector3f.h"
#include "Vector4f.h"


// Matrix4f
// Implementation copied from Apple's GLKit framework.
typedef union {
	struct {
		float m00, m01, m02, m03;
		float m10, m11, m12, m13;
		float m20, m21, m22, m23;
		float m30, m31, m32, m33;
	};
	float m[16];
} Matrix4f;


// Prototypes
static inline Matrix4f makeMatrix4f(float m00, float m01, float m02, float m03,
									float m10, float m11, float m12, float m13,
									float m20, float m21, float m22, float m23,
									float m30, float m31, float m32, float m33);
static inline Matrix4f makeMatrix4fFromXRotation(float radians);
static inline Matrix4f makeMatrix4fFromYRotation(float radians);
static inline Matrix4f makeMatrix4fFromZRotation(float radians);
static inline Matrix4f makeMatrix4fFromFrustum(float left, float right, float top, float bottom, float near, float far);
static inline Matrix4f makeMatrix4fFromSymFrustum(float left, float top, float near, float far);
static inline Matrix4f makeMatrix4fFromProjection(float fovy, float aspect, float nearZ, float farZ);
static inline Matrix4f makeMatrix4fFromOrtho(float left, float right, float top, float bottom, float near, float far);
static inline Matrix4f makeMatrix4fModelToWorld(float modX, float modY, float modZ, float forX, float forY, float forZ, float upX, float upY, float upZ);
static inline Matrix4f makeMatrix4fWorldToCamera(float eyeX, float eyeY, float eyeZ, float centreX, float centreY, float centreZ, float upX, float upY, float upZ);
static inline Matrix4f makeMatrix4fFromLookAt(float eyeX, float eyeY, float eyeZ,
											  float centerX, float centerY, float centerZ,
											  float upX, float upY, float upZ);
static inline Matrix4f makeMatrix4fFromLookAt(Vector3f p, Vector3f l, Vector3f u);
static inline Matrix4f makeMatrix4fFromLookAtWithPositiveZ(float eyeX, float eyeY, float eyeZ,
														   float centerX, float centerY, float centerZ,
														   float upX, float upY, float upZ);
static inline Matrix4f makeMatrix4fFromReverseLookAt(float eyeX, float eyeY, float eyeZ,
											  float centerX, float centerY, float centerZ,
											  float upX, float upY, float upZ);
static inline Matrix4f makeMatrix4fFromReverseLookAt(Vector3f p, Vector3f l, Vector3f u);
static inline Matrix4f makeMatrix4fIdentity();
static inline std::string makeStringFromMatrix4f(Matrix4f m);

static inline Matrix4f multiplyMatrixByMatrix4f(Matrix4f leftMatrix, Matrix4f rightMatrix);
// Dep.
static inline Vector4f multiplyMatrixByVector4f(Matrix4f m, Vector4f v);
static inline Vector4f multiplyVectorByMatrix4f(Vector4f v, Matrix4f m);



// Actual Functions
Matrix4f makeMatrix4f(float m00, float m01, float m02, float m03,
					float m10, float m11, float m12, float m13,
					float m20, float m21, float m22, float m23,
					float m30, float m31, float m32, float m33)
{
	Matrix4f m = { {
		m00, m01, m02, m03,
		m10, m11, m12, m13,
		m20, m21, m22, m23,
		m30, m31, m32, m33 } };
	return m;
}


Matrix4f makeMatrix4fFromXRotation(float radians)
{
	float cos = cosf(radians);
	float sin = sinf(radians);

	Matrix4f m = { {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, cos, sin, 0.0f,
		0.0f, -sin, cos, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
			} };
	return m;
}


Matrix4f makeMatrix4fFromYRotation(float radians)
{
	float cos = cosf(radians);
	float sin = sinf(radians);

	Matrix4f m = { {
		cos, 0.0f, -sin, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		sin, 0.0f, cos, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
			} };
	return m;
}


Matrix4f makeMatrix4fFromZRotation(float radians)
{
	float cos = cosf(radians);
	float sin = sinf(radians);

	Matrix4f m = { {
		cos, sin, 0.0f, 0.0f,
		-sin, cos, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
			} };
	return m;
}


// Implementation from: http://www.songho.ca/opengl/gl_projectionmatrix.html
Matrix4f makeMatrix4fFromFrustum(float l, float r, float t, float b, float n, float f)
{
	Matrix4f m = { {
		2*n / (r-l), 	0, 				0, 					0,
		0, 				2*n / (t-b), 	0, 					0,
		(r+l) / (r-l), 	(t+b) / (t-b),	-(f+n) / (f-n),		-1,
		0, 				0, 				-(2*f*n) / (f-n), 	0
	} };
	return m;
}


// Implementation from: http://www.songho.ca/opengl/gl_projectionmatrix.html
Matrix4f makeMatrix4fFromSymFrustum(float r, float t, float n, float f)
{
	Matrix4f m = {{
		n / r, 0, 0, 0,
		0, n / t, 0, 0,
		0, 0, -(f + n) / (f - n), -1,
		0, 0, -(2 * f*n) / (f - n), 0
	}};
	return m;
}


Matrix4f makeMatrix4fFromProjection(float fovy, float aspect, float nearZ, float farZ)
{
    float cotan = 1.0f / tanf(fovy / 2.0f);

	Matrix4f m = {{
		cotan / aspect, 0.0f, 0.0f, 0.0f,
		0.0f, cotan, 0.0f, 0.0f,
		0.0f, 0.0f, (farZ + nearZ) / (nearZ - farZ), -1.0f,
		0.0f, 0.0f, (2.0f * farZ * nearZ) / (nearZ - farZ), 0.0f
	}};

    return m;
}


Matrix4f makeMatrix4fFromOrtho(float left, float right, float top, float bottom, float near, float far)
{
	float ral = right + left;
	float rsl = right - left;
	float tab = top + bottom;
	float tsb = top - bottom;
	float fan = far + near;
	float fsn = far - near;
	
	Matrix4f m = {{
		2.0f / rsl, 0.0f, 0.0f, 0.0f,
		0.0f, 2.0f / tsb, 0.0f, 0.0f,
		0.0f, 0.0f, -2.0f / fsn, 0.0f,
		-ral / rsl, -tab / tsb, -fan / fsn, 1.0f
	}};
	
	return m;
}


// Implementation copied from Apple's GLKit library
// Transform matrix to move the origin of all coordinates to the given location with center being on the negative z axis
Matrix4f makeMatrix4fFromLookAt(float eyeX, float eyeY, float eyeZ,
						  float centerX, float centerY, float centerZ,
						  float upX, float upY, float upZ)
{
	Vector3f ev = { { eyeX, eyeY, eyeZ } };
	Vector3f cv = { { centerX, centerY, centerZ } };
	Vector3f uv = { { upX, upY, upZ } };
    Vector3f n = normalize3f(addVector3f(ev, negateVector3f(cv)));
    Vector3f u = normalize3f(crossProduct3f(uv, n));
    Vector3f v = crossProduct3f(n, u);

	Matrix4f m = { {
		u.v[0], v.v[0], n.v[0], 0.0f,
		u.v[1], v.v[1], n.v[1], 0.0f,
		u.v[2], v.v[2], n.v[2], 0.0f,
		dotProduct3f(negateVector3f(u), ev),
		dotProduct3f(negateVector3f(v), ev),
		dotProduct3f(negateVector3f(n), ev),
		1.0f
	} };
	return m;
}


Matrix4f makeMatrix4fFromLookAt(Vector3f p, Vector3f l, Vector3f u)
{
	return makeMatrix4fFromLookAt(p.x, p.y, p.z, l.x, l.y, l.z, u.x, u.y, u.z);
}



// Calculates the world to camera matrix with a positive z axis
Matrix4f makeMatrix4fFromLookAtWithPositiveZ(float eyeX, float eyeY, float eyeZ,
								float centerX, float centerY, float centerZ,
								float upX, float upY, float upZ)
{
	Vector3f ev = { { eyeX, eyeY, eyeZ } };
	Vector3f cv = { { centerX, centerY, centerZ } };
	Vector3f uv = { { upX, upY, upZ } };
    Vector3f n = normalize3f(addVector3f(ev, negateVector3f(cv)));
    Vector3f u = normalize3f(crossProduct3f(uv, n));
    Vector3f v = crossProduct3f(n, u);

	Matrix4f m = { {
		u.v[0], v.v[0], n.v[0], 0.0f,
		u.v[1], v.v[1], n.v[1], 0.0f,
		u.v[2], v.v[2], n.v[2], 0.0f,
		dotProduct3f(negateVector3f(u), ev),
		dotProduct3f(negateVector3f(v), ev),
		dotProduct3f(negateVector3f(n), ev),
		1.0f
			} };
	Matrix4f rotate = makeMatrix4fFromYRotation((float) M_PI);
	m = multiplyMatrixByMatrix4f(m, rotate);
	return m;
}


// Calculates the matrix that will convert a point from one space to another given
// the perspective of the current space
Matrix4f makeMatrix4fFromReverseLookAt(float eyeX, float eyeY, float eyeZ,
											  float centerX, float centerY, float centerZ,
											  float upX, float upY, float upZ)
{
	// The model will face the z+ axis with y+ pointing upwards
	// So find those axis in model space, then create a new matrix using those values
	Matrix4f worldToModel = makeMatrix4fFromLookAtWithPositiveZ(eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ);
//	Matrix4f worldToModel = makeMatrix4fFromLookAt(eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ);
	Vector4f orig = { { 0, 0, 0, 1 } },
		posZ = { { 0, 0, 1, 1 } },
		posY = { { 0, 1, 0, 0 } };
	// Need to multiply a vector by this matrix
	Vector4f origModel = multiplyMatrixByVector4f(worldToModel, orig);
	Vector4f posZModel = multiplyMatrixByVector4f(worldToModel, posZ);
	Vector4f posYModel = multiplyMatrixByVector4f(worldToModel, posY);

	Matrix4f modelToWorld = makeMatrix4fFromLookAtWithPositiveZ(origModel.x, origModel.y, origModel.z, posZModel.x, posZModel.y, posZModel.z, posYModel.x, posYModel.y, posYModel.z);
	return modelToWorld;
}


Matrix4f makeMatrix4fFromReverseLookAt(Vector3f p, Vector3f l, Vector3f u)
{
	return makeMatrix4fFromReverseLookAt(p.x, p.y, p.z, l.x, l.y, l.z, u.x, u.y, u.z);
}


Matrix4f makeMatrix4fIdentity()
{
	Matrix4f m = { {
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
			} };
	return m;
}


std::string makeStringFromMatrix4f(Matrix4f m)
{
	std::string s = "";
	for (int i = 0; i < 4; i++)
	{
		s += "[";
		for (int j = 0; j < 4; j++)
		{
			if (j != 0)
				s += ", ";
			s += std::to_string(m.m[i * 4 + j]);
		}
		s += "]\n";
	}
	return s;
}


Matrix4f multiplyMatrixByMatrix4f(Matrix4f matrixLeft, Matrix4f matrixRight)
{
	Matrix4f m;
	m.m[0]  = matrixLeft.m[0] * matrixRight.m[0]  + matrixLeft.m[4] * matrixRight.m[1]  + matrixLeft.m[8] * matrixRight.m[2]   + matrixLeft.m[12] * matrixRight.m[3];
	m.m[4]  = matrixLeft.m[0] * matrixRight.m[4]  + matrixLeft.m[4] * matrixRight.m[5]  + matrixLeft.m[8] * matrixRight.m[6]   + matrixLeft.m[12] * matrixRight.m[7];
	m.m[8]  = matrixLeft.m[0] * matrixRight.m[8]  + matrixLeft.m[4] * matrixRight.m[9]  + matrixLeft.m[8] * matrixRight.m[10]  + matrixLeft.m[12] * matrixRight.m[11];
	m.m[12] = matrixLeft.m[0] * matrixRight.m[12] + matrixLeft.m[4] * matrixRight.m[13] + matrixLeft.m[8] * matrixRight.m[14]  + matrixLeft.m[12] * matrixRight.m[15];
	
	m.m[1]  = matrixLeft.m[1] * matrixRight.m[0]  + matrixLeft.m[5] * matrixRight.m[1]  + matrixLeft.m[9] * matrixRight.m[2]   + matrixLeft.m[13] * matrixRight.m[3];
	m.m[5]  = matrixLeft.m[1] * matrixRight.m[4]  + matrixLeft.m[5] * matrixRight.m[5]  + matrixLeft.m[9] * matrixRight.m[6]   + matrixLeft.m[13] * matrixRight.m[7];
	m.m[9]  = matrixLeft.m[1] * matrixRight.m[8]  + matrixLeft.m[5] * matrixRight.m[9]  + matrixLeft.m[9] * matrixRight.m[10]  + matrixLeft.m[13] * matrixRight.m[11];
	m.m[13] = matrixLeft.m[1] * matrixRight.m[12] + matrixLeft.m[5] * matrixRight.m[13] + matrixLeft.m[9] * matrixRight.m[14]  + matrixLeft.m[13] * matrixRight.m[15];
	
	m.m[2]  = matrixLeft.m[2] * matrixRight.m[0]  + matrixLeft.m[6] * matrixRight.m[1]  + matrixLeft.m[10] * matrixRight.m[2]  + matrixLeft.m[14] * matrixRight.m[3];
	m.m[6]  = matrixLeft.m[2] * matrixRight.m[4]  + matrixLeft.m[6] * matrixRight.m[5]  + matrixLeft.m[10] * matrixRight.m[6]  + matrixLeft.m[14] * matrixRight.m[7];
	m.m[10] = matrixLeft.m[2] * matrixRight.m[8]  + matrixLeft.m[6] * matrixRight.m[9]  + matrixLeft.m[10] * matrixRight.m[10] + matrixLeft.m[14] * matrixRight.m[11];
	m.m[14] = matrixLeft.m[2] * matrixRight.m[12] + matrixLeft.m[6] * matrixRight.m[13] + matrixLeft.m[10] * matrixRight.m[14] + matrixLeft.m[14] * matrixRight.m[15];
	
	m.m[3]  = matrixLeft.m[3] * matrixRight.m[0]  + matrixLeft.m[7] * matrixRight.m[1]  + matrixLeft.m[11] * matrixRight.m[2]  + matrixLeft.m[15] * matrixRight.m[3];
	m.m[7]  = matrixLeft.m[3] * matrixRight.m[4]  + matrixLeft.m[7] * matrixRight.m[5]  + matrixLeft.m[11] * matrixRight.m[6]  + matrixLeft.m[15] * matrixRight.m[7];
	m.m[11] = matrixLeft.m[3] * matrixRight.m[8]  + matrixLeft.m[7] * matrixRight.m[9]  + matrixLeft.m[11] * matrixRight.m[10] + matrixLeft.m[15] * matrixRight.m[11];
	m.m[15] = matrixLeft.m[3] * matrixRight.m[12] + matrixLeft.m[7] * matrixRight.m[13] + matrixLeft.m[11] * matrixRight.m[14] + matrixLeft.m[15] * matrixRight.m[15];
	return m;
	
	// Code is incorrect
//	Matrix4f r{ {
//		m.m00 * n.m00 + m.m01 * n.m10 + m.m02 * n.m20 + m.m03 * n.m30,
//		m.m00 * n.m01 + m.m01 * n.m11 + m.m02 * n.m21 + m.m03 * n.m31,
//		m.m00 * n.m02 + m.m01 * n.m12 + m.m02 * n.m22 + m.m03 * n.m32,
//		m.m00 * n.m03 + m.m01 * n.m13 + m.m02 * n.m23 + m.m03 * n.m33,
//
//		m.m10 * n.m00 + m.m11 * n.m10 + m.m12 * n.m20 + m.m13 * n.m30,
//		m.m10 * n.m01 + m.m11 * n.m11 + m.m12 * n.m21 + m.m13 * n.m31,
//		m.m10 * n.m02 + m.m11 * n.m12 + m.m12 * n.m22 + m.m13 * n.m32,
//		m.m10 * n.m03 + m.m11 * n.m13 + m.m12 * n.m23 + m.m13 * n.m33,
//
//		m.m20 * n.m00 + m.m21 * n.m10 + m.m22 * n.m20 + m.m23 * n.m30,
//		m.m20 * n.m01 + m.m21 * n.m11 + m.m22 * n.m21 + m.m23 * n.m31,
//		m.m20 * n.m02 + m.m21 * n.m12 + m.m22 * n.m22 + m.m23 * n.m32,
//		m.m20 * n.m03 + m.m21 * n.m13 + m.m22 * n.m23 + m.m23 * n.m33,
//
//		m.m30 * n.m00 + m.m31 * n.m10 + m.m32 * n.m20 + m.m33 * n.m30,
//		m.m30 * n.m01 + m.m31 * n.m11 + m.m32 * n.m21 + m.m33 * n.m31,
//		m.m30 * n.m02 + m.m31 * n.m12 + m.m32 * n.m22 + m.m33 * n.m32,
//		m.m30 * n.m03 + m.m31 * n.m13 + m.m32 * n.m23 + m.m33 * n.m33,
//			} };
//	return r;
}


Vector4f multiplyMatrixByVector4f(Matrix4f m, Vector4f v)
{
	Vector4f r = {{
		m.m00 * v.x + m.m10 * v.y + m.m20 * v.z + m.m30 * v.w,
		m.m01 * v.x + m.m11 * v.y + m.m21 * v.z + m.m31 * v.w,
		m.m02 * v.x + m.m12 * v.y + m.m22 * v.z + m.m32 * v.w,
		m.m03 * v.x + m.m13 * v.y + m.m23 * v.z + m.m33 * v.w,
	}};
	return r;
}


Vector4f multiplyVectorByMatrix4f(Vector4f v, Matrix4f m)
{
	Vector4f r = { {
		m.m00 * v.x + m.m10 * v.y + m.m20 * v.z + m.m30 * v.w,
		m.m01 * v.x + m.m11 * v.y + m.m21 * v.z + m.m31 * v.w,
		m.m02 * v.x + m.m12 * v.y + m.m22 * v.z + m.m32 * v.w,
		m.m03 * v.x + m.m13 * v.y + m.m23 * v.z + m.m33 * v.w,
			} };
	return r;
}


#endif /* defined(__GraphicsEngine_v3__Matrix4f__) */
