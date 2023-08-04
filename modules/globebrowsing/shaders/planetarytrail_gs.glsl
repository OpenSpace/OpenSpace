/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#version __CONTEXT__

layout (lines_adjacency) in;
layout (triangle_strip, max_vertices = 6) out;

uniform vec2  viewport;
uniform float lineWidth;
uniform int nPoints;

in VertexData {
  float opacity;
} gs_in[];

out VertexData {
  float opacity;
} gs_out;

void main()
{
  vec4 p0 = gl_in[0].gl_Position;
  p0.xyz /= p0.w;

  vec4 p1 = gl_in[1].gl_Position/gl_in[1].gl_Position.w;
  p1.xyz /= p1.w;

  vec4 p2 = gl_in[2].gl_Position/gl_in[2].gl_Position.w;
  p2.xyz /= p2.w;

  vec4 p3 = gl_in[3].gl_Position/gl_in[3].gl_Position.w;
  p3.xyz /= p3.w;

  p0.xy = (p0.xy + 1) * 0.5 * viewport;
  p1.xy = (p1.xy + 1) * 0.5 * viewport;
  p2.xy = (p2.xy + 1) * 0.5 * viewport;
  p3.xy = (p3.xy + 1) * 0.5 * viewport;

  vec2 p = normalize(p1.xy - p0.xy);
  vec2 np = vec2(-p.y, p.x);

  vec2 l = normalize(p2.xy - p1.xy);
  vec2 nl = vec2(-l.y, l.x);

  vec2 n = normalize(p3.xy - p2.xy);
  vec2 nn = vec2(-n.y, n.x);

  vec2 m1 = normalize(nl + np);
  vec2 m2 = normalize(nl + nn);

  vec4 pos;

  pos = p1;
  pos.xy += np * lineWidth * 0.5;
  pos.xy = 2 * pos.xy / viewport - 1;
  pos.xyz *= pos.w;

  gs_out.opacity = gs_in[1].opacity;
  gl_Position = pos;
  EmitVertex();

  pos = p1;
  pos.xy += nl * lineWidth * 0.5;
  pos.xy = 2 * pos.xy / viewport - 1;
  pos.xyz *= pos.w;

  gs_out.opacity = gs_in[1].opacity;
  gl_Position = pos;
  EmitVertex();

  pos = p1;
  pos.xy -= nl * lineWidth * 0.5;
  pos.xy = 2 * pos.xy / viewport - 1;
  pos.xyz *= pos.w;

  gs_out.opacity = gs_in[1].opacity;
  gl_Position = pos;
  EmitVertex();

  pos = p2;
  pos.xy += nl * lineWidth * 0.5;
  pos.xy = 2 * pos.xy / viewport - 1;
  pos.xyz *= pos.w;

  gs_out.opacity = gs_in[2].opacity;
  gl_Position = pos;
  EmitVertex();

  pos = p2;
  pos.xy -= nl * lineWidth * 0.5;
  pos.xy = 2 * pos.xy / viewport - 1;
  pos.xyz *= pos.w;

  gs_out.opacity = gs_in[2].opacity;
  gl_Position = pos;
  EmitVertex();

  if (gl_PrimitiveIDIn < nPoints - 3) {
    pos = p2;
    pos.xy -= nn * lineWidth * 0.5;
    pos.xy = 2 * pos.xy / viewport - 1;
    pos.xyz *= pos.w;

    gs_out.opacity = gs_in[2].opacity;
    gl_Position = pos;
    EmitVertex();
  }

  EndPrimitive();
}
