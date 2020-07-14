/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
in vec4 vs_color;
in float vs_depth;


//float lightSpeed = 299792458.0;
/*
in vec4 vs_positionScreenSpace;
in vec4 vs_gPosition;
in vec4 vs_color;
in float distanceFromStart;
in float timeSinceStart;
in float transmissionTime;
in float lightTravelTime;
*/
Fragment getFragment() {
    if (vs_color.a == 0) {
        discard;
    }

    vec4 fragColor = vs_color;

    Fragment frag;
    frag.depth = vs_depth;
    
    /*
     // the distance the first signal transmission has travelled 
    float distLightTravelStart = lightSpeed * timeSinceStart;
    // the distance the last signal transmission has travelled 
    float distLightTravelEnd = lightSpeed * (timeSinceStart-transmissionTime);

    if(distanceFromStart < distLightTravelStart && distanceFromStart > distLightTravelEnd){
        // calculate how fast the signal segments travel within transmission
        float distFlowTravelStart = distLightTravelStart * flowSpeedFactor;

        float signalSize = distLightTravelStart-distLightTravelEnd;
        float signalSegmentSize = pow(lightTravelTime,segmentSizeFactor) * lightSpeed;

        // make the spacing dependent on the segment size
        float spacing = signalSegmentSize * spacingSizeFactor;
       // alpha = getSegmentOpacity(signalSegmentSize,spacing, distFlowTravelStart, distLightTravelStart, distLightTravelEnd);
        alpha = 0.5;
    }
   
    frag.color = vec4(fragColor.xyz, alpha);
     */
     frag.color = fragColor;
    // G-Buffer
    frag.gPosition = vec4(0.0); //vs_gPosition;
    // There is no normal here
    // TODO: Add the correct normal if necessary (JCC)
    frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

    return frag;
}
