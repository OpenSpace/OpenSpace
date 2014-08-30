#version 440


const vec2 corners[4] = { 
    vec2(0.0, 1.0), 
	vec2(0.0, 0.0), 
	vec2(1.0, 1.0), 
	vec2(1.0, 0.0) 
};

layout(points) in;
layout(points, max_vertices = 1) out; // Draw points 
//layout(triangle_strip, max_vertices = 4) out; // Draw quads
layout(location = 2) in vec3  vs_brightness[];
layout(location = 2) out vec3 ge_brightness[];

uniform vec4 campos;

float spriteSize = 0.000005; // set here for now.
out vec2 texCoord;

void main(){
	ge_brightness[0] = vs_brightness[0];


	float distToPoint = 1;//(50.0*(length(gl_in[0].gl_Position - campos)) );

	float radius = 1.f;
	// EMIT POINT
	// pointscaling not enabled yet
	gl_Position = gl_in[0].gl_Position;
	/*float dist = length(gl_Position.xyz - campos.xyz);
	float psize = (radius*1000000.f) / dist;*/
	gl_PointSize = 1.f;
	
	EmitVertex();
	EndPrimitive();
}
