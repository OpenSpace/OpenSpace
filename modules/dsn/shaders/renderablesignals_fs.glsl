/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include "fragment.glsl"
#include "floatoperations.glsl"

in vec4 vs_positionScreenSpace;
in vec4 vs_gPosition;
in vec4 vs_color;
in float distanceFromStart;
in float timeSinceStart;

float lightSpeed = 299792458.0; // expressed in m/s
float signalSizeFactor = 2000;
float baseOpacity = 0.9;

Fragment getFragment() {

    Fragment frag;
    frag.depth = vs_positionScreenSpace.w;
    //frag.blend = BLEND_MODE_ADDITIVE;

    // the distance the light has travelled since 
    // start of signal transmission
    float distLightTravel = lightSpeed * timeSinceStart;

    // signal segment size
    float signalSize = signalSizeFactor * lightSpeed;
    float edgeLength = signalSize*0.5;

    // Make smooth transitions for both ends of the segment
    float smoothFront = smoothstep(distLightTravel+edgeLength,
                                   distLightTravel-edgeLength,
                                   distanceFromStart);
    float smoothBack = smoothstep(distLightTravel-signalSize-edgeLength, 
                                  distLightTravel-signalSize+edgeLength,
                                  distanceFromStart);

    frag.color = vec4(vs_color.xyz, min(smoothFront,smoothBack)+baseOpacity);

    // G-Buffer
    // JCC: The depthCorrection here is a temporary tweak
    // to fix precision problems.
    // LOVISA: We get issues with the coloring of our signals
    // within the atmosphere, the depth correction moves and 
    // smoothens this transition to black. 
    // This is a temporary fix until normals are handled in
    // view space in the atmosphere shader.
    vec4 depthCorrection = vec4(0.0,0.0,9000,0.0);
    frag.gPosition = vs_gPosition + depthCorrection;

    // For rendering inside earth atmosphere we need to set a normal for our line
    // Todo: calculate normal correctly 
    // currently normal is in object space
    frag.gNormal= vec4(0.0, 0.0, -1.0, 1.0); 

    return frag;
}
