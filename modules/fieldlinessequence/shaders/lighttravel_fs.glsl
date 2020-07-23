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
in flat int render_mode;
in vec2 vs_st;

uniform sampler2D spriteTexture;
//uniform bool additiveBlending;
float lightSpeed = 299792458.0;

in vec4 vs_positionScreenSpace;
in vec4 vs_gPosition;
in float distanceFromStart;
in float timeSinceStart;
in float transmissionTime;
in float lightTravelTime;

/*
uniform float distanceFromStart;

uniform float transmissionTime;
uniform float lightTravelTime;
*/
/*
uniform float flowSpeedFactor;
uniform float segmentSizeFactor;
uniform float spacingSizeFactor;
uniform float fadeFactor;
uniform float baseOpacity;
*/
float flowSpeedFactor = 1000000000;
float segmentSizeFactor = 200;
float spacingSizeFactor = 2000;
float fadeFactor = 0.2;
float baseOpacity = 0.5;
/*
float getSegmentOpacity(const float segmentSize, 
                        const float spacing, 
                        const float distFlowTravelStart,
                        const float signalTravelStart,
                        const float signalTravelEnd) {
    
    float fadeLength = segmentSize * fadeFactor;

    // if fadeLength is zero, the smoothstep does not work, return straight away
    if(fadeFactor < 0.001f)
    {
        return 1.0f;
    }

    int firstVisibleSegmentIndex = 0;

    float flowParticleDistance = distFlowTravelStart;

    // find the first flow segment that is within our visibility  
    while (flowParticleDistance > signalTravelStart){
        firstVisibleSegmentIndex = firstVisibleSegmentIndex + 1;
        flowParticleDistance = distFlowTravelStart - firstVisibleSegmentIndex*(segmentSize+spacing);
    }

    int lastVisibleSegmentIndex = firstVisibleSegmentIndex;
    // find the last flow segment that is within our visibility  
    while (flowParticleDistance > signalTravelEnd){
        lastVisibleSegmentIndex = lastVisibleSegmentIndex + 1;
        flowParticleDistance = distFlowTravelStart - lastVisibleSegmentIndex*(segmentSize+spacing);
    }

    // make a compensation so the first visible segment is not cut off
    firstVisibleSegmentIndex = firstVisibleSegmentIndex-1;

    // look through if our current fragment is within a colored segment and return its opacity
    for(int i = firstVisibleSegmentIndex; i < lastVisibleSegmentIndex; i++ )
    {
        float segmentStart =  distFlowTravelStart-i*(segmentSize+spacing);
        float segmentEnd =  distFlowTravelStart-(i+1)*(segmentSize) -i*spacing;
        
        // if within a colored segment, calculate opacity
        if (distanceFromStart < segmentStart && distanceFromStart > segmentEnd){   
            
            // Make smooth transitions at both ends of the segment
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
*/

Fragment getFragment() {
    if (vs_color.a == 0) {
        discard;
    }

    vec4 fragColor = vs_color;

    Fragment frag;
    frag.depth = vs_depth;
    
    /*
    if(vs_st.x == 0){
    discard;
    }
    else{
    if(gl_FrontFacing) {
        frag.color = texture(texture1, vs_st);
    }
    else{
        frag.color = texture(texture1, vec2(1 - vs_st.s, vs_st.t));
    }
    }
    */
    if(render_mode == 3){
    frag.color = texture(spriteTexture, gl_PointCoord) * vec4(1, 1, 1, 0.1);
    //if(gl_FrontFacing) {
    //    frag.color = texture(spriteTexture, vs_st);
       // frag.color = texture(spriteTexture, gl_PointCoord);
    //}
   /* else{
       // frag.color = texture(spriteTexture, vec2(1 - vs_st.s, vs_st.t));
       frag.color = vec4(0.1, 0.1, 0.1, 1.0);
    }
    */
    //frag.blend = BLEND_MODE_ADDITIVE;
    }
    else{
    frag.color = fragColor;
    }
    /*
     // the distance the first signal transmission has travelled 
    float distLightTravelStart = lightSpeed * timeSinceStart;
    // the distance the last signal transmission has travelled 
    float distLightTravelEnd = lightSpeed * (timeSinceStart-transmissionTime);

     float alpha = 0.0f;
    if(distanceFromStart < distLightTravelStart && distanceFromStart > distLightTravelEnd){
        // calculate how fast the signal segments travel within transmission
        float distFlowTravelStart = distLightTravelStart * flowSpeedFactor;

        float signalSize = distLightTravelStart-distLightTravelEnd;
        float signalSegmentSize = pow(lightTravelTime,segmentSizeFactor) * lightSpeed;

        // make the spacing dependent on the segment size
        float spacing = signalSegmentSize * spacingSizeFactor;
        alpha = getSegmentOpacity(signalSegmentSize,spacing, distFlowTravelStart, distLightTravelStart, distLightTravelEnd);
       // alpha = 0.5;
    }
   */
    //frag.color = vec4(fragColor.xyz, alpha);
     
    
    // G-Buffer
    //frag.gPosition = vec4(0.0); //vs_gPosition;
    //vec4 depthCorrection = vec4(0.0,0.0,9000,0.0);
    //frag.gPosition = vs_gPosition + depthCorrection;
    // There is no normal here
    // TODO: Add the correct normal if necessary (JCC)
    //frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
    frag.gPosition  = vs_gPosition;
    frag.gNormal    = vec4(0.0, 0.0, 0.0, 1.0);

    return frag;
}
