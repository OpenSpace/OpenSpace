/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2023 Inviwo Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *********************************************************************************/

#include "utils/structs.glsl"
#include "utils/pickingutils.glsl"

layout(location = 5) in int in_tetraFaceId;

uniform GeometryParameters geometry;
uniform CameraParameters camera;

out Fragment {
    smooth vec4 worldPosition;
    smooth vec3 position;
    flat vec4 color;
    flat int tetraFaceId;

    flat vec3 camPosData;
} out_vert;

void main(void) {
    // gl_VertexID
    out_vert.color = vec4(pickingIndexToColor(in_tetraFaceId + 1), 1.0);
    out_vert.tetraFaceId = in_tetraFaceId;
    out_vert.camPosData = vec3(geometry.worldToData * vec4(camera.position, 1.0));

    out_vert.position = in_Vertex.xyz;
    out_vert.worldPosition = geometry.dataToWorld * vec4(in_Vertex.xyz, 1.0);
    gl_Position = camera.worldToClip * out_vert.worldPosition;
}
