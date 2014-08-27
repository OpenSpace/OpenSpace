#version 440

const vec2 corners[4] = { 
    vec2(0.0, 1.0), 
	vec2(0.0, 0.0), 
	vec2(1.0, 1.0), 
	vec2(1.0, 0.0) 
};

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

uniform mat4 projection;
float spriteSize = 0.05; // set here for now.
out vec2 texCoord;

void main()
{
	// make quad + corresp. UV coordinates.
    for(int i=0; i<4; ++i){
        vec4 pos    = gl_in[0].gl_Position;                  //start with point position
        pos.xy     += spriteSize * (corners[i] - vec2(0.5)); //add corner position
        gl_Position = projection * pos;                      //complete transformation
        texCoord    = corners[i];                            //use corner as texCoord
        EmitVertex();
    }
	EndPrimitive();
}
