#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

layout(location = 1) in vec3 position; // Stream 0
layout(location = 2) in vec3 vertexColor;

out vec3 fragmentColor;

void main() {
	fragmentColor = vertexColor;
	gl_Position = proj * view * model * vec4(position, 1.0);
}
