#version 330

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

uniform vec3 solidColor;

in vec3 position;

out vec3 fragColor;

void main() {
	fragColor = solidColor;
	gl_Position = proj * view * model * vec4(position, 1.0);
}
