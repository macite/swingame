#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

in vec3 position;

void main() {
	gl_Position = proj * view * model * vec4(position, 1.0);
}
