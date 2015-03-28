#version 330
#define MAX_NUMBER_OF_LIGHTS 12

struct LightData
{
	vec3 position;
	vec3 intensities;
	float attenuation;
	float ambientCoefficient;
//	sampler2D shadowMap;
};

struct MaterialData
{
	vec3 diffuseColor;
	vec3 specularColor;
	float specularExponent;
	float specularIntensity;
	sampler2D texture;
	bool useTexture;
};


uniform mat4 model;
in		mat4 normalModel;
uniform vec3 cameraPosition;

uniform MaterialData material;
uniform LightData lights[MAX_NUMBER_OF_LIGHTS];
uniform int numberOfLights;

in vec3 fragCoord;
in vec3 fragNormal;
in vec2 fragTexCoord;

out vec4 finalColor;


void main() {
	vec3 normal = normalize(mat3(normalModel) * fragNormal);
	vec3 surfacePos = vec3(model) * fragCoord;
	vec3 surfaceToCamera = normalize(cameraPosition - surfacePos);

	vec4 surfaceColor;
	if (material.useTexture)
	{
		surfaceColor = texture(material.texture, fragTexCoord);
	}
	else
	{
		surfaceColor = vec4(material.diffuseColor, 1);
	}
	
	finalColor = vec4(0, 0, 0, 1);
	
	for (int i = 0; i < numberOfLights; i++)
	{
		vec3 surfaceToLight = normalize(lights[i].position - surfacePos);
		
		// Ambient
		vec3 ambient = lights[i].ambientCoefficient * lights[i].intensities * surfaceColor.rgb;
		
		// Diffuse
		// Max prevents light appearing on the back face of the surface
		float diffuseCoefficient = max(0.0, dot(normal, surfaceToLight));
		vec3 diffuse = diffuseCoefficient * surfaceColor.rgb * lights[i].intensities;
		
		// Specular
		float specularCoefficient = 0.0;
		vec3 specular = vec3(0, 0, 0);
		// Specular can only occur if diffuse is not zero
		if (diffuseCoefficient > 0.0)
		{
			specularCoefficient = material.specularIntensity * pow(max(0.0, dot(surfaceToCamera, reflect(-surfaceToLight, normal))), material.specularExponent);
			specular = specularCoefficient * material.specularColor * lights[i].intensities;
		}
		
		// Attenuation
		float distanceToLight = length(lights[i].position - surfacePos);
		float attenuation = 1.0 / (1.0 + lights[i].attenuation * pow(distanceToLight, 2));
		
		finalColor += vec4(ambient + attenuation * (diffuse + specular), surfaceColor.a);
	}
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom

