#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

uniform vec3 solidColor;

layout(location = 1) in vec3 position; // Stream 0

out vec3 fragmentColor;

void main() {
	fragmentColor = solidColor;
	gl_Position = proj * view * model * vec4(position, 1.0);
}
