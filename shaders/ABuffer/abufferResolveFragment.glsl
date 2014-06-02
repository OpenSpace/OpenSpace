#version 430

// texture bindings
layout (binding = 0, r32ui) uniform uimage2D anchorPointerTexture;
layout (binding = 1, rgba32ui) uniform uimageBuffer fragmentTexture;

// atomic buffer bindings
layout (binding = 0, offset = 0) uniform atomic_uint atomicCounterBuffer;

// uniforms
uniform int SCREEN_WIDTH;
uniform int SCREEN_HEIGHT;

in vec2 texCoord;
out vec4 color;
#include "abufferStruct.hglsl"

#define MAX_FRAGMENTS 16
ABufferStruct_t fragments[MAX_FRAGMENTS];

#include "abufferSort.hglsl"


vec4 blend(vec4 current_color, vec4 new_color) {
	return mix(current_color, new_color, new_color.a);
}

vec4 calculate_final_color(uint frag_count) {
	
	vec4 final_color = vec4(0);
	for(uint i = 0; i < frag_count; i++) {
		ABufferStruct_t item = fragments[i];
		vec4 frag_color = _col_(item);
		final_color = blend(final_color, frag_color);
	}

	return final_color;

}


void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);
    color = calculate_final_color(frag_count);

    //color = vec4(float(frag_count) / 5.0, 0.0, 0.0, 1.0);
}