#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 mvp;

in vec3 position;

void main() {
	gl_Position = mvp * vec4(position, 1.0);
}
