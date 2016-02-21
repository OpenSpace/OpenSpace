#version 430 core

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;
uniform vec4 campos;
uniform mat4 camrot;
uniform vec2 scaling;
uniform vec4 objpos;
uniform float time;

layout(location = 0) in vec3 vertPosition;
layout(location = 1) in vec4 vertColor;

out vec4 fColor;
out vec3 vs_position;
out float s;

const float k = 10.0;

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

vec4 psc_to_meter(vec4 v1, vec2 v2) {
	float factor = v2.x * pow(k,v2.y + v1.w);
	return vec4(v1.xyz * factor, 1.0);
}

vec4 psc_scaling(vec4 v1, vec2 v2) {
	float ds = v2.y - v1.w;
	if(ds >= 0) {
		return vec4(v1.xyz * v2.x * pow(k,v1.w), v2.y);
	} else {
		return vec4(v1.xyz * v2.x * pow(k,v2.y), v1.w);
	}
}

vec4 pscTransform(vec4 vertexPosition, vec4 cameraPosition, vec2 scaling, mat4 modelTransform) {
	vec3 local_vertex_pos = mat3(modelTransform) * vertexPosition.xyz;
	//vec4 lvp = ModelTransform * vertexPosition;
	
	// PSC addition; local vertex position and the object power scaled world position
	vec4 position = psc_addition(vec4(local_vertex_pos,vertexPosition.w),objpos);
	//position = psc_addition(lvp,objpos);
	
	// PSC addition; rotated and viewscaled vertex and the cmaeras negative position
	position = psc_addition(position,vec4(-cameraPosition.xyz,cameraPosition.w));
	
	// rotate the camera
	local_vertex_pos =  mat3(camrot) * position.xyz;
	position = vec4(local_vertex_pos, position.w);
	//position =  camrot* position;

	// rescales the scene to fit inside the view frustum
	// is set from the main program, but these are decent values
	// scaling = vec2(1.0, -8.0);

	// project using the rescaled coordinates,
	//vec4 vs_position_rescaled = psc_scaling(position, scaling);
	return psc_to_meter(position, scaling);
}

void main() {
	fColor = vertColor;
	// gl_Position = modelViewProjection*vec4(vertPosition, 1.0);


	vec4 p = vec4(vertPosition,0);
	
	vec4 position = pscTransform(p, campos, scaling, modelTransform);
	vs_position = position.xyz;
	s = position.w;

	// project the position to view space
	gl_Position =  modelViewProjection * position;
}