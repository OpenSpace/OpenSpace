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
in float transmissionTime;
in float lightTravelTime;

// light speed expressed in m/s
float lightSpeed = 299792458.0; 

// the maximum number of segments to be drawn
int maxNumSegments = 10000; //int(ceil(lightTravelTime* 10.0f));

uniform float flowSpeedFactor;
uniform float segmentSizeFactor;
uniform float spacingSizeFactor;
uniform float fadeFactor;
uniform float baseOpacity;

float getSegmentOpacity(const float segmentSize, 
                        const float spacing, 
                        const float distSignTravelStart) {
    
    float fadeLength = segmentSize * fadeFactor;

    // if fadeLength is zero, the smoothstep does not work, return straight away
    if(fadeFactor < 0.001f)
    {
        return 1.0f;
    }

    for(int i = 0; i < maxNumSegments; i++ )
    {
    
        float segmentStart =  distSignTravelStart-i*(segmentSize+spacing);
        float segmentEnd =  distSignTravelStart-(i+1)*(segmentSize) -i*spacing;
        
        // if within a colored segment, calculate opacity
        if (distanceFromStart < segmentStart && distanceFromStart > segmentEnd){   
            
            // Make smooth transitions for both ends of the segment
            float smoothFront = smoothstep(segmentStart+fadeLength,
                                        segmentStart-fadeLength,
                                        distanceFromStart);

            float smoothBack = smoothstep(segmentEnd-fadeLength, 
                                        segmentEnd+fadeLength,
                                        distanceFromStart);
                
            //generate opacity factor
            return vs_color.a*(baseOpacity + min(smoothFront,smoothBack));            
        }

    } // end for loop

    // if within a spacing
    return vs_color.a*baseOpacity;
}


Fragment getFragment() {

    Fragment frag;
    frag.depth = vs_positionScreenSpace.w;
    //frag.blend = BLEND_MODE_ADDITIVE;

    // the distance the first signal transmission has travelled 
    float distLightTravelStart = lightSpeed * timeSinceStart;
    // the distance the last signal transmission has travelled 
    float distLightTravelEnd = lightSpeed * (timeSinceStart-transmissionTime);
    float signalSize = distLightTravelStart-distLightTravelEnd;
    float signalSegmentSize = pow(lightTravelTime,segmentSizeFactor) * lightSpeed;

    float alpha = 0.0f;
    // if within the transmission time, change the opacity
    if(distanceFromStart < distLightTravelStart && distanceFromStart > distLightTravelEnd){
        // calculate how fast the signal segments travel within transmission
        float distSignTravelStart = distLightTravelStart * flowSpeedFactor;
        // make the spacing dependent on the segment size
        float spacing = signalSegmentSize * spacingSizeFactor;
        alpha = getSegmentOpacity(signalSegmentSize,spacing, distSignTravelStart);
    }

    frag.color = vec4(vs_color.rgb, alpha);

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
