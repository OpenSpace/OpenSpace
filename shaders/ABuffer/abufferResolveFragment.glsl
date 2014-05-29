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

#define MAX_FRAGMENTS 16
uvec4 fragments[MAX_FRAGMENTS];

int build_local_fragments_list() {
	uint current;
	int frag_count = 0;

	current = imageLoad(anchorPointerTexture, ivec2(gl_FragCoord.xy)).x;

	while(current != 0 && frag_count < MAX_FRAGMENTS) {
		uvec4 item = imageLoad(fragmentTexture, int(current));
		current = item.x;

		fragments[frag_count] = item;

		frag_count++;
	}

	return frag_count;
}

void sort_fragments_list(uint frag_count) {
	uint i,j;
	uvec4 tmp;

	// INSERTION SORT
	for(i = 1; i < frag_count; ++i) {
		tmp = fragments[i];
		for(j = i; j > 0 && tmp.z > fragments[j-1].z; --j) {
			fragments[j] = fragments[j-1];
		}
		fragments[j] = tmp;
	}
}

vec4 blend(vec4 current_color, vec4 new_color) {
	return mix(current_color, new_color, new_color.a);
}

vec4 calculate_final_color(uint frag_count) {
	
	vec4 final_color = vec4(0);
	for(uint i = 0; i < frag_count; i++) {
		uvec4 item = fragments[i];
		vec4 frag_color = unpackUnorm4x8(item.y);
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