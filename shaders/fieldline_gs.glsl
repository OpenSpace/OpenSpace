
#version __CONTEXT__

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;

layout(lines) in;
layout(line_strip, max_vertices = 2) out;

in vec4 vs_color[];
in vec4 vs_position[];
out vec4 gs_color;
out vec4 gs_position;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    gs_color = vs_color[0];
    for(int i = 0; i < 2; i++) {        
        // gs_position = gl_in[i].gl_Position;
        // gl_Position = gl_in[i].gl_Position;

        // calculate psc position
        vec4 tmp = gl_in[i].gl_Position;
        vec4 position = pscTransform(tmp, modelTransform);
        gs_position = tmp;

        // project the position to view space
        position =  modelViewProjection * position;
        gl_Position =  z_normalization(position);
        EmitVertex();
  }
  EndPrimitive();
}

// uniform mat4 ModelviewProjection;
// layout(lines_adjacency) in;
// layout(location = 0) in vec3 vPosition[4]; // Four inputs since we're using GL_LINE_STRIP_ADJACENCY
// // layout(location = 2) in vec3 vNormal[4];   // Orientation vectors are necessary for consistent alignment
// layout(triangle_strip, max_vertices = 24) out;

// out vec4 in_color;

// vec4 prismoid[8]; // Scratch space for the eight corners of the prismoid

// void emit(int a, int b, int c, int d) {
//     gl_Position = prismoid[a]; EmitVertex();
//     gl_Position = prismoid[b]; EmitVertex();
//     gl_Position = prismoid[c]; EmitVertex();
//     gl_Position = prismoid[d]; EmitVertex();
//     EndPrimitive();
// } 

// void main() {
//     // Compute orientation vectors for the two connecting faces:
//     vec3 vNormal = vec3(0, 0, 1);

//     vec3 p0, p1, p2, p3;
//     p0 = vPosition[0]; p1 = vPosition[1];
//     p2 = vPosition[2]; p3 = vPosition[3];
//     vec3 n0 = normalize(p1-p0);
//     vec3 n1 = normalize(p2-p1);
//     vec3 n2 = normalize(p3-p2);
//     vec3 u = normalize(n0+n1);
//     vec3 v = normalize(n1+n2);

//     // Declare scratch variables for basis vectors:
//     vec3 i,j,k; float r = 0.1;

//     // Compute face 1 of 2:
//     // j = u; i = vNormal[1]; k = cross(i, j); i *= r; k *= r;
//     j = u; i = vNormal; k = cross(i, j); i *= r; k *= r;
//     prismoid[0] = ModelviewProjection * vec4(p1 + i + k, 1);
//     prismoid[1] = ModelviewProjection * vec4(p1 + i - k, 1);
//     prismoid[2] = ModelviewProjection * vec4(p1 - i - k, 1);
//     prismoid[3] = ModelviewProjection * vec4(p1 - i + k, 1);

//     // Compute face 2 of 2:
//     // j = v; i = vNormal[2]; k = cross(i, j); i *= r; k *= r;
//     j = v; i = vNormal; k = cross(i, j); i *= r; k *= r;
//     prismoid[4] = ModelviewProjection * vec4(p2 + i + k, 1);
//     prismoid[5] = ModelviewProjection * vec4(p2 + i - k, 1);
//     prismoid[6] = ModelviewProjection * vec4(p2 - i - k, 1);
//     prismoid[7] = ModelviewProjection * vec4(p2 - i + k, 1);

//     // Emit the six faces of the prismoid:
//     emit(0,1,3,2); emit(5,4,6,7);
//     emit(4,5,0,1); emit(3,2,7,6);
//     emit(0,3,4,7); emit(2,1,6,5);
// }
