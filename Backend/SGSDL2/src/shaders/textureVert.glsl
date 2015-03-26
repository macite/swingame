#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

in vec3 position;
in vec2 texCoord;

out vec3 fragColor;
out vec2 fragTexCoord;

void main() {
	fragTexCoord = texCoord;
	gl_Position = proj * view * model * vec4(position, 1.0);
}
