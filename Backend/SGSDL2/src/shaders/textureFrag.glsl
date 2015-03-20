#version 330

in vec3 fragmentColor;
in vec2 fragTexCoord;

out vec4 finalColor;

uniform sampler2D tex;

void main() {
	finalColor = texture(tex, fragTexCoord);
}
