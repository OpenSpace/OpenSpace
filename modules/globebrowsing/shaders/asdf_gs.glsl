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

void main()
{
  vec4 p0 = gl_in[0].gl_Position;
  vec4 p1 = gl_in[1].gl_Position;
  vec4 p2 = gl_in[2].gl_Position;
  vec4 p3 = gl_in[3].gl_Position;

  vec2 p = normalize((p1.xy - p0.xy) * viewport);
  vec2 pn = vec2(-p.y, p.x);

  vec2 l = normalize((p2.xy - p1.xy) * viewport);
  vec2 ln = vec2(-l.y, l.x);

  vec2 n = normalize((p3.xy - p2.xy) * viewport);
  vec2 nn = vec2(-n.y, n.x);

  // P2
  gl_Position = p1 + vec4(pn * lineWidth / viewport, 0, 0);
  EmitVertex();

  // V0
  gl_Position = p1 + vec4(ln * lineWidth / viewport, 0, 0);
  EmitVertex();

  // V1
  gl_Position = p1 - vec4(ln * lineWidth / viewport, 0, 0);
  EmitVertex();

  // V2
  gl_Position = p2 + vec4(ln * lineWidth / viewport, 0, 0);
  EmitVertex();

  // V3
  gl_Position = p2 - vec4(ln * lineWidth / viewport, 0, 0);
  EmitVertex();

  // N1
  gl_Position = p2 - vec4(nn * lineWidth / viewport, 0 , 0);
  EmitVertex();

  EndPrimitive();
}

// Below does not work when angles become too large between adjacent points..
// hence solution above to generate extra triangles
// vec4 va[4];
// for (int i=0; i<4; ++i)
// {
//     va[i] = gl_in[i].gl_Position;
//     va[i].xyz /= va[i].w;
//     va[i].xy = (va[i].xy + 1.0) * 0.5 * viewport;
// }

// vec2 v_line   = normalize(va[2].xy - va[1].xy);
// vec2 nv_line  = vec2(-v_line.y, v_line.x);
// vec2 v_pred   = normalize(va[1].xy - va[0].xy);
// vec2 v_succ   = normalize(va[3].xy - va[2].xy);
// // vec2 v_miter1 = 0.5 * (nv_line + vec2(-v_pred.y, v_pred.x));
// // vec2 v_miter2 = 0.5 *(nv_line + vec2(-v_succ.y, v_succ.x));
// vec2 v_miter1 = normalize(nv_line + vec2(-v_pred.y, v_pred.x));
// vec2 v_miter2 = normalize(nv_line + vec2(-v_succ.y, v_succ.x));

// vec4 pos;

// pos = va[1];
// pos.xy += v_miter1 * lineWidth;
// // pos.xy += v_miter1 * lineWidth / dot(v_miter1, nv_line);
// // pos.xy += nv_line * lineWidth;

// pos.xy = pos.xy / viewport * 2.0 - 1.0;
// pos.xyz *= pos.w;
// gl_Position = pos;
// EmitVertex();

// pos = va[1];
// pos.xy -= v_miter1 * lineWidth;
// // pos.xy -= v_miter1 * lineWidth / dot(v_miter1, nv_line);
// // pos.xy -= nv_line * lineWidth;

// pos.xy = pos.xy / viewport * 2.0 - 1.0;
// pos.xyz *= pos.w;
// gl_Position = pos;
// EmitVertex();

// pos = va[2];
// pos.xy += v_miter2 * lineWidth;
// // pos.xy += v_miter2 * lineWidth / dot(v_miter2, nv_line);
// // pos.xy += nv_line * lineWidth;

// pos.xy = pos.xy / viewport * 2.0 - 1.0;
// pos.xyz *= pos.w;
// gl_Position = pos;
// EmitVertex();

// pos = va[2];
// pos.xy -= v_miter2 * lineWidth;
// // pos.xy -= v_miter2 * lineWidth / dot(v_miter2, nv_line);
// // pos.xy -= nv_line * lineWidth;
// pos.xy = pos.xy / viewport * 2.0 - 1.0;
// pos.xyz *= pos.w;
// gl_Position = pos;
// EmitVertex();

// EndPrimitive();