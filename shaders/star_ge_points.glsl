#version 440
layout(points) in;
layout(points, max_vertices = 1) out; // Draw points 
//layout(triangle_strip, max_vertices = 4) out; // Draw quads
layout(location = 2) in vec3  vs_brightness[];
layout(location = 2) out vec3 ge_brightness[];

float spriteSize = 0.1; // set here for now.
out vec2 texCoord;
out float scale;

in vec4 psc_position[];
in vec4 campos[];

float k = 10.f;
vec4 psc_addition(vec4 v1, vec4 v2) {
	float ds = v2.w - v1.w;
	if(ds >= 0) {
		float p = pow(k,-ds);
		return vec4(v1.x*p + v2.x, v1.y*p + v2.y, v1.z*p + v2.z, v2.w);
	} else {
		float p = pow(k,ds);
		return vec4(v1.x + v2.x*p, v1.y + v2.y*p, v1.z + v2.z*p, v1.w);
	}
}
float log10( float x ){  return log(x) / log(10);   }

void main(){
	ge_brightness[0] = vs_brightness[0];
	
	float distToPoint = 1;

	float radius = 2f;
	// EMIT POINT 
	gl_Position = gl_in[0].gl_Position;
		
	// right now only threshing with absolute magnitude. 
	float absMag = 1.0f;
	if(vs_brightness[0].x <  0.0) absMag = 3;
	if(vs_brightness[0].x < -3.0) absMag = 6;
	if(vs_brightness[0].x < -6.0) absMag = 9;
	
//	float M  = vs_brightness[0][0];                                 // get ABSOLUTE magnitude (x param)
	float M  = vs_brightness[0][2]; // if NOT running test-target.
	vec4 cam = vec4(-campos[0].xyz, campos[0].w);                  // get negative camera position   
    // does swizzle work?? FFS!  do it manually:
	vec4 pos = psc_position[0];                                    // get OK star position
	
	vec4 result = psc_addition(pos, cam);                          // compute vec from camera to position
	float x, y, z, w;
	x = result[0];
	y = result[1];
	z = result[2];
	w = result[3];
	                                                               // I dont trust the legnth function at this point
	vec2 pc = vec2(sqrt(x*x +y*y + z*z), result[3]);               // form vec2 
	 
	pc[0] *= 0.324077929;                                          // convert meters -> parsecs
	pc[1] += -18;
	
	float pc_psc = pc[0] * pow(10, pc[1]);                         // psc scale out
	float apparent = (M - 5.0f * (1.f - log10(pc_psc)));           // formula, get appMagnitude. 
     
	vec4 P = gl_in[0].gl_Position;
	 
	float weight = 0.1f; 										    // otherwise this takes over.
	float depth  = -P.z;
	//depth       *= pow(apparent,6);
	//if(round(apparent) > 10)
	scale = -apparent*weight;
	
	gl_PointSize = -apparent*0.3;
	
	EmitVertex();
	EndPrimitive();
}
