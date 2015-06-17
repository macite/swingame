#version 330
#define MAX_NUMBER_OF_LIGHTS 16

struct LightData
{
	vec3 position;
	vec3 direction;
	mat4 transform;
	vec3 intensities;
	float attenuation;
	float ambientCoefficient;
	float cosOuterCone;
	float cosInnerCone;
	int lightType;
	bool castsShadows;
	int shadowMapLevel;
};

uniform LightData lights[MAX_NUMBER_OF_LIGHTS];
uniform int numberOfLights;

uniform mat4 model;
out		mat4 normalModel;
uniform mat4 view;
uniform mat4 proj;
//uniform vec3 cameraPosition;

in vec3 position;
in vec3 normal;
in vec2 texCoord;

out vec3 fragCoord;
out vec3 fragNormal;
out vec2 fragTexCoord;
out vec4 fragShadowCoords[MAX_NUMBER_OF_LIGHTS];


void main() {
	normalModel = transpose(inverse(model));
	
	fragNormal = normal;
	fragCoord = position;
	fragTexCoord = texCoord;
	
	gl_Position = proj * view * model * vec4(position, 1.0);
	
	// Shadow coords
	for (int i = 0; i < numberOfLights; i++)
	{
		fragShadowCoords[i] = lights[i].transform * model * vec4(position, 1);
//		fragShadowCoords[i] = vec4(fragShadowCoords[i].xyz * 0.5 + 0.5, fragShadowCoords[i].w);
	}
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom

