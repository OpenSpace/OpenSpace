#version 440
const vec2 corners[4] = { 
    vec2(0.0, 1.0), 
	vec2(0.0, 0.0), 
	vec2(1.0, 1.0), 
	vec2(1.0, 0.0) 
};

layout(points) in;
//layout(points, max_vertices = 1) out;
layout(triangle_strip, max_vertices = 4) out;

uniform vec4 campos;

float spriteSize = 0.000007; // set here for now.
out vec2 texCoord;

void main(){
	//TODO: implement distance metric.
	float distToPoint = 1;//(50.0*(length(gl_in[0].gl_Position - campos)) );

	// EMIT QUAD
		for(int i=0; i<4; ++i){
			vec4 pos    = gl_in[0].gl_Position;                 
			pos.xy     += spriteSize*distToPoint *(corners[i] - vec2(0.5)); 
			gl_Position = pos;                      			
			texCoord    = corners[i];                           
			EmitVertex();
		}
		EndPrimitive();
}