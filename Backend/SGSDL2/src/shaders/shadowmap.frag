#version 330

layout(location = 0) out float fragmentdepth;

void main() {
	fragmentdepth = gl_FragCoord.z;
}
