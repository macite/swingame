#version 330
#define MAX_NUMBER_OF_LIGHTS 12


uniform mat4 model;
out		mat4 normalModel;
uniform mat4 view;
uniform mat4 proj;
uniform vec3 cameraPosition;

//uniform MaterialData material;
//uniform LightData lights[12];
//uniform int numberOfLights;


in vec3 position;
in vec3 normal;
in vec2 texCoord;

out vec3 fragCoord;
out vec3 fragNormal;
out vec2 fragTexCoord;


void main() {
	normalModel = transpose(inverse(model));
	fragNormal = normal;
	fragCoord = position;
	fragTexCoord = texCoord;
	
	gl_Position = proj * view * model * vec4(position, 1.0);
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom

