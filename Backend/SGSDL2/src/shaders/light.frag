#version 330
#define MAX_NUMBER_OF_LIGHTS 12
#define M_PI 3.1415926535897932384626433832795

// Lighting type
#define LIGHT_TYPE_POINT		1
#define LIGHT_TYPE_DIRECTIONAL	2
#define LIGHT_TYPE_SPOT			3

struct LightData
{
	vec3 position;
	vec3 direction;
	vec3 intensities;
	float attenuation;
	float ambientCoefficient;
	float cosOuterCone;
	float cosInnerCone;
	int lightType;
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


// Returns the modifier for how bright the light is (used only for spot lights)
// Will return 1 if the light is not a spot light
float calculateSpotIntensityModifier(LightData light, vec3 surfaceToLight)
{
	if (light.lightType != LIGHT_TYPE_SPOT)
	{
		return 1;
	}
	
	// The cos angle
	float cosAngle = dot(surfaceToLight, -normalize(light.direction));
	// Distance from the inner cone
	cosAngle -= light.cosOuterCone;
//	// Normalized distance from the inner cone
	cosAngle /= (light.cosInnerCone - light.cosOuterCone);
//	// Clamped to within range
	cosAngle = clamp(cosAngle, 0, 1);
	return cosAngle;
}


void main() {
	vec3 normal = normalize(mat3(normalModel) * fragNormal);
	vec3 surfacePos = vec3(model) * fragCoord;
	vec3 surfaceToCamera = normalize(cameraPosition - surfacePos);

	// Determine the surface color based on whether it is textured on not
	vec4 surfaceColor;
	if (material.useTexture)
	{
		surfaceColor = texture(material.texture, fragTexCoord);
	}
	else
	{
		surfaceColor = vec4(material.diffuseColor, 1);
	}
	
	// Loop through all of the lights in the scene
	finalColor = vec4(0, 0, 0, 1);
	for (int i = 0; i < numberOfLights; i++)
	{
		// Vector from the surface to the supposed light position
		vec3 surfaceToLight;
		if (lights[i].lightType == LIGHT_TYPE_DIRECTIONAL)
		{
			// Vector is the same as the direction of the light
			surfaceToLight = normalize(lights[i].direction);
		}
		else
		{
			// For other lights it is calculated normally
			surfaceToLight = normalize(lights[i].position - surfacePos);
		}
		
		// The intensity of the light based on its direction (used only for spot lights)
		// angle = acos(
		float spotIntensity = calculateSpotIntensityModifier(lights[i], surfaceToLight);
		
		// Ambient
		vec3 ambient = lights[i].ambientCoefficient * lights[i].intensities * surfaceColor.rgb;
		
		// Diffuse
		// Max prevents light appearing on the back face of the surface
		float diffuseCoefficient = max(0.0, dot(normal, surfaceToLight)) * spotIntensity;
		vec3 diffuse = diffuseCoefficient * surfaceColor.rgb * lights[i].intensities;
		
		// Specular
		float specularCoefficient = 0.0;
		vec3 specular = vec3(0, 0, 0);
		// Specular can only occur if diffuse is not zero
		if (diffuseCoefficient > 0.0)
		{
			// specInt * lightInt * (surfToCam . reflectionVec) ^ specExpo
			specularCoefficient = material.specularIntensity * spotIntensity * pow(max(0.0, dot(surfaceToCamera, reflect(-surfaceToLight, normal))), material.specularExponent);
			specular = specularCoefficient * material.specularColor * lights[i].intensities;
		}
		
		// Attenuation
		float distanceToLight = length(lights[i].position - surfacePos);
		float attenuation = 1.0 / (1.0 + lights[i].attenuation * pow(distanceToLight, 2));
		
		finalColor += vec4(ambient + attenuation * (diffuse + specular), surfaceColor.a);
//		finalColor += vec4(ambient + attenuation * (diffuse + specular), surfaceColor.a);
//		finalColor += vec4(surfaceColor.rgb * spotIntensity, 1);
	}
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom

