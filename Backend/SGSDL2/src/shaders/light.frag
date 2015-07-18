#version 330
#define MAX_NUMBER_OF_LIGHTS 16
#define M_PI 3.1415926535897932384626433832795

// Lighting type
#define LIGHT_TYPE_POINT		1
#define LIGHT_TYPE_DIRECTIONAL	2
#define LIGHT_TYPE_SPOT			3

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

struct MaterialData
{
	vec3 diffuseColor;
	sampler2D diffuseTexture;
	vec3 specularColor;
	sampler2D specularTexture;
	float specularExponent;
	float specularIntensity;
	sampler2D normalMap;
};

uniform MaterialData material;
uniform LightData lights[MAX_NUMBER_OF_LIGHTS];
uniform int numberOfLights;
uniform sampler2DArrayShadow shadowMap;

uniform mat4 model;
uniform mat4 normalModel;
uniform vec3 cameraLocation;

in vec3 fragLocation;
in vec3 fragNormal;
in vec2 fraguv;
in vec3 globalFragLocation;
in vec4 fragShadowCoords[MAX_NUMBER_OF_LIGHTS];

out vec4 finalColor;


float getShadowFactor(int lightIndex)
{
	LightData light = lights[lightIndex];
	vec4 shadowCoord = fragShadowCoords[lightIndex];

	// Divide by w coord if it is projected
	if (light.type != LIGHT_TYPE_DIRECTIONAL)
	{
		shadowCoord = vec4(shadowCoord.x / shadowCoord.w,
						   shadowCoord.y / shadowCoord.w,
						   shadowCoord.z / shadowCoord.w,
						   shadowCoord.w);
	}

	float shadowBias = 0.005;
	float shadowMapValue = texture(shadowMap, vec4(shadowCoord.xy, light.shadowMapLevel, shadowCoord.z - shadowBias));
	return shadowMapValue;
}


// Returns the modifier for how bright the light is (used only for spot lights)
// Will return 1 if the light is not a spot light
float getSpotFactor(LightData light, vec3 surfaceToLight)
{
	if (light.type != LIGHT_TYPE_SPOT)
	{
		return 1;
	}

	// The cos angle
	float cosAngle = dot(surfaceToLight, -normalize(light.forward));
	// Distance from the inner cone
	cosAngle -= light.cosOuterCone;
	// Normalized distance from the inner cone
	cosAngle /= (light.cosInnerCone - light.cosOuterCone);
	// Clamped to within range
	cosAngle = clamp(cosAngle, 0, 1);
	return cosAngle;
}


vec4 getDiffuseBase()
{
	return vec4(material.diffuseColor, 1) * texture(material.diffuseTexture, fraguv);
}


vec3 getSpecularBase()
{
	return material.specularColor, 1 * texture(material.specularTexture, fraguv).xyz;
}


vec3 getNormal()
{
	return (normalModel * vec4(fragNormal * (texture(material.normalMap, fraguv) * 2.0 - 1.0).xyz, 0)).xyz;
}


vec3 getPhongColor(int lightIndex)
{
	LightData light = lights[lightIndex];
	vec3 diffuseBase = getDiffuseBase().rgb;
	vec3 specularBase = getSpecularBase();
	vec3 normal = getNormal();
	float surfaceToLightDist = length(light.location - globalFragLocation);
	vec3 surfaceToLight = normalize(light.location - globalFragLocation);
	vec3 surfaceToCamera = normalize(cameraLocation - globalFragLocation);

	vec3 ambient = light.ambientCoefficient * light.intensities * diffuseBase;

	// Diffuse and specular are zero if the light is too far away
	vec3 diffuse = vec3(0, 0, 0);
	vec3 specular = vec3(0, 0, 0);
	if (surfaceToLightDist <= light.attenuation)
	{
		diffuse = max(0.0, dot(surfaceToLight, normal)) * diffuseBase * light.intensities;

		specular = pow(max(0.0, dot(reflect(-surfaceToLight, normal), surfaceToCamera)), material.specularExponent) * material.specularIntensity * material.specularColor * light.intensities;
	}

	float attenuation = 1.0 / (1.0 + pow(surfaceToLightDist * 5 / light.attenuation, 2));

	return ambient + attenuation * getShadowFactor(lightIndex) * getSpotFactor(light, surfaceToLight) * (diffuse + specular);
//	return vec3(1, 0, 0);
}


void main() {
	// Loop through all of the lights in the scene
	finalColor = vec4(0, 0, 0, getDiffuseBase().a);
	for (int i = 0; i < numberOfLights; i++)
	{
		finalColor += vec4(getPhongColor(i), 0);
	}
}


// If you want an array of textures, must use a texture array
// You can iterate over an array of an opaque type in gl 4.3+
// A sampler can be a member of a struct, but the struct can only be used as a unifrom
