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
//#include "floatoperations.glsl"

uniform vec3 color;
uniform float opacity = 1.0;

uniform float lineFade;
// uniform int numberOfSegments;


in vec4 viewSpacePosition;
in float vs_position_w;

in float periodFraction_f;
in float offsetPeriods;
in float vertexID_f;

// debuggers :
// in float offset;
// in float epoch;
// in float period;
// in flat double tajm;
Fragment getFragment() {
    // float offsetPeriods = offset / period;
    // This is now done in the fragment shader instead
    // to make smooth movement between vertecies.
    // We want vertexDistance to be double up to this point, I think. 
    // (hence the unnessesary float to float conversion)
    float vertexDistance = periodFraction_f - offsetPeriods;
    float vertexDistance_f = float(vertexDistance);
    
    // This is the alternative way of calculating 
    // the offsetPeriods: (vertexID_perOrbit/nrOfSegments_f)
    // float vertexID_perOrbit = mod(vertexID_f, numberOfSegments);
    // float nrOfSegments_f = float(numberOfSegments);
    // float vertexDistance = periodFraction_f - (vertexID_perOrbit/nrOfSegments_f); 

    if (vertexDistance_f < 0.0) {
        vertexDistance_f += 1.0;
    }

    float invert = pow((1.0 - vertexDistance_f), lineFade);
    float fade = clamp(invert, 0.0, 1.0);

    // Currently even fully transparent lines can occlude other lines, thus we discard
    // these fragments since debris and satellites are rendered so close to each other
    if (fade < 0.05) {
        discard;
    }
    Fragment frag;

    // Use additive blending for some values to make the discarding less abrupt
    if (fade < 0.15) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }

    frag.color = vec4(color, fade * opacity);
    frag.depth = vs_position_w;
    frag.gPosition = viewSpacePosition;
    frag.gNormal = vec4(1, 1, 1, 0);


    // to debug using colors use this if-statment.
    // float ep = 0.01;
    // if( fract(vertexID_f) < ep ){ //periodFraction < ep
    //      frag.color = vec4(1, 0, 0, 1);
    // }

    return frag;
}




