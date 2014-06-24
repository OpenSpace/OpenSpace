#version 430 core

uniform int volumeType;

in vec3 vPosition;
in vec3 worldPosition;
out vec4 fragColor;

#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
 
void main() {
	fragColor = vec4(vPosition+0.5, 0.3);

	ABufferStruct_t frag;
	_col_(frag, fragColor);
	_z_(frag, gl_FragCoord.z);
	_type_(frag, volumeType);
	_pos_(frag, vec4(worldPosition,0));
	addToBuffer(frag);

	//discard;
}