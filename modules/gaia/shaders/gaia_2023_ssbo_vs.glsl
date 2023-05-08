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

#include "floatoperations.glsl"
// #define FLT_MIN 1.175494351e-38

layout (std430) buffer ssbo_idx_data { 
  int starsPerChunk[];
};

layout (std430) buffer ssbo_comb_data { 
  float allData[];
};
 
out vec2 vs_brightness;
out vec4 vs_gPosition;
out float vs_starDistFromSun;
out float vs_cameraDistFromSun;

//thesis 2023
out float vs_otherData;
uniform int columnIndex;

out float cameraDistance; 
uniform vec3 cameraPos;

uniform dmat4 model;
uniform dmat4 view;
uniform dmat4 projection;
uniform float time; 
uniform int renderOption;
uniform int maxStarsPerNode;
uniform int valuesPerStar;
uniform int nChunksToRender;



// uniform vec2 posXThreshold;
// uniform vec2 posYThreshold;
// uniform vec2 posZThreshold;
// uniform vec2 gMagThreshold;
// uniform vec2 bpRpThreshold;
// uniform vec2 distThreshold;

// Keep in sync with gaiaoptions.h:RenderOption enum
// const int RENDEROPTION_STATIC = 0;
// const int RENDEROPTION_COLOR = 1;
// const int RENDEROPTION_MOTION = 2; 
const float EPS = 1e-5;
const float Parsec = 3.0856776e16;


// Use binary search to find the chunk containing our star ID.
int findChunkId(int left, int right, int id) {
  while (left <= right) {
    int middle = (left + right) / 2;
    int firstStarInChunk = starsPerChunk[middle];
    if (left == right || (firstStarInChunk <= id && id < starsPerChunk[middle+1])) {
      return middle;
    }
    else if (id < firstStarInChunk) {
      // Go smaller
      right = middle - 1;
    }
    else {
      // Go bigger
      left = middle + 1;
    }
  }
  return -1;
}


void main() {
  // Fetch our data.
  int chunkId = findChunkId(0, nChunksToRender - 1, gl_VertexID);
  // Fail safe - this should never happen!
  if (chunkId == -1) {
    vs_gPosition = vec4(0.0);    
    gl_Position = vec4(0.0);
    return;
  }
  int placeInChunk = gl_VertexID - starsPerChunk[chunkId]; 
  int firstStarInChunk = valuesPerStar * maxStarsPerNode * chunkId; // Chunk offset
  int nStarsInChunk = starsPerChunk[chunkId + 1] - starsPerChunk[chunkId]; // Stars in current chunk.
  // Remove possible duplicates
  if (nStarsInChunk <= 0) {
    vs_gPosition = vec4(0.0);    
    gl_Position = vec4(0.0);
    return;
  }
  
  int startOfPos = firstStarInChunk + placeInChunk * 3;
  vec3 in_position = vec3(allData[startOfPos], allData[startOfPos + 1], allData[startOfPos + 2]);
  vec2 in_brightness = vec2(0.0);
  vec3 in_velocity = vec3(0.0);

  int startOfCol = firstStarInChunk + nStarsInChunk * 3 + placeInChunk * 2; 
  in_brightness = vec2(allData[startOfCol], allData[startOfCol + 1]); //Gmag, color

  int startOfVel = firstStarInChunk + nStarsInChunk * 5 + placeInChunk * 3; //
  in_velocity = vec3(allData[startOfVel], allData[startOfVel + 1], allData[startOfVel + 2]);

  vs_brightness = in_brightness;

  // int otherDataIdx = firstStarInChunk + nStarsInChunk * (3+2+3) + (columnIndex-8) + placeInChunk * (valuesPerStar - 8);

  //Due to the data layout we must use special rules for pos, col, and velocity indices.
  int otherDataIdx = 0;
  if(columnIndex < 3) //Index 0,1,2 are for the positional data, x,y,z.
  {
    //Move to first star, move to this star's positional data, columnIndex gets the corret x,y, or z component. 
    otherDataIdx = firstStarInChunk + placeInChunk * 3 + columnIndex;
  }
  else if(columnIndex < 5) //Index 3,4 are the color data: absolut magnitude, color
  {
    //Move to first star, move past the positional data, move to this star's color data, (columnIndex - startindex) finds the correct component
    int startIndex = 3;
    otherDataIdx = firstStarInChunk + nStarsInChunk * 3 + placeInChunk * 2 + (columnIndex - startIndex);
  }
  else if(columnIndex < 8) //Index 5,6,7 are the velocity data, vx,vy,vz
  {
    //Move to first star, move past positional and color data, move to this star's velocity data,
    //(columnindex - start index) finds the correct vx, vy, or vz component.
    int startIndex = 5;
    otherDataIdx = firstStarInChunk + nStarsInChunk * 5 + placeInChunk * 3 + (columnIndex - startIndex);
  }
  else { //Column index is an optional data parameter.
    //Column index starts at 8 and there are only 1 value per star for every column.
    //Therefore we can immediately move past all data using nstarsinchunk * columnindex and find our correct value with placeInChunk
    otherDataIdx = firstStarInChunk + nStarsInChunk * columnIndex + placeInChunk;
  }
  vs_otherData = allData[otherDataIdx];


  // Convert kiloParsec to meter
  vec4 objectPosition = vec4(in_position * 1000 * Parsec, 1.0); //TODO remove science?
  // Add velocity given in km/s 
  objectPosition.xyz += time * in_velocity * 1000;

  // Apply camera transforms
  dvec4 viewPosition = view * model * objectPosition;
  vec4 sunPosition = vec4(view * model * dvec4(0.0f, 0.0f, 0.0f, 1.0f));

  //Measure distance between camera and star in view space
  cameraDistance = safeLength(cameraPos - vec3(viewPosition));

  vs_starDistFromSun = safeLength(objectPosition);
  vs_cameraDistFromSun = safeLength(sunPosition);

  // Remove stars without position, happens when VBO chunk is stuffed with zeros.
  // Has to be done in Geometry shader because Vertices cannot be discarded here.
  // Stars that have NaN values as their optional render value are also discarded.
  if (length(in_position) > EPS && !isnan(vs_otherData)){
    vs_gPosition = vec4(model * objectPosition);    
    gl_Position = vec4(projection * viewPosition);
  }
  else {
    vs_gPosition = vec4(0.0);    
    gl_Position = vec4(0.0);
  }
}
