#version 430 core

in vec3 vPosition;
out vec4 fragColor;

#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
 
void main() {
	fragColor = vec4(vPosition+0.5, 1.0);

	ABufferStruct_t frag;
	_col_(frag, fragColor);
	_z_(frag, gl_FragDepth);
	_type_(frag, 0);
	addToBuffer(frag);

	discard;
}