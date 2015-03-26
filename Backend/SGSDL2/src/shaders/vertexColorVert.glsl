#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

in vec3 position;
in vec3 vertexColor;

out vec3 fragColor;

void main() {
	fragColor = vertexColor;
	gl_Position = proj * view * model * vec4(position, 1.0);
}
