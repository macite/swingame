#version 330
#define MAX_NUMBER_OF_LIGHTS 16

struct LightData
{
	vec3 location;
	vec3 forward;
	mat4 transform;
	vec3 intensities;
	float attenuation;
	float ambientCoefficient;
	float cosOuterCone;
	float cosInnerCone;
	bool castsShadows;
	int shadowMapLevel;
	int type;
};

uniform LightData lights[MAX_NUMBER_OF_LIGHTS];
uniform int numberOfLights;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 mvp;

in vec3 location;
in vec3 normal;
in vec2 uv;

out vec3 fragLocation;
out vec3 fragNormal;
out vec2 fraguv;
out vec3 globalFragLocation;
out vec4 fragShadowCoords[MAX_NUMBER_OF_LIGHTS];

void main() {

	fragLocation = location;
	fragNormal = normal;
	fraguv = uv;
	globalFragLocation = vec3(model * vec4(location, 1));
	// Shadow coords
	for (int i = 0; i < numberOfLights; i++)
	{
		fragShadowCoords[i] = lights[i].transform * model * vec4(location, 1);
	}

	gl_Position = proj * view * model * vec4(location, 1.0);
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom
