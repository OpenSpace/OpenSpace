/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/galaxy/milkywaypointsconversiontask.h>
#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/volumesampler.h>
#include <fstream>
#include <iostream>

namespace openspace {
namespace dataconverter {
    


MilkyWayPointsConversionTask::MilkyWayPointsConversionTask(
    const std::string& inFilename,
    const std::string& outFilename)
    : _inFilename(inFilename)
    , _outFilename(outFilename) {}


void MilkyWayPointsConversionTask::perform(const std::function<void(float)>& onProgress) {
    std::ifstream in(_inFilename, std::ios::in);
    std::ofstream out(_outFilename, std::ios::out | std::ios::binary);
      
    std::string format;
    int64_t nPoints;
    in >> format >> nPoints;


 
    size_t nFloats = nPoints * 7;

    float* pointData = new float[nFloats];

    float x, y, z, r, g, b, a;
    for (size_t i = 0; i < nPoints; ++i) {
        in >> x >> y >> z >> r >> g >> b >> a;
        if (in.good()) {
            pointData[i * 7 + 0] = x;
            pointData[i * 7 + 1] = y;
            pointData[i * 7 + 2] = z;
            pointData[i * 7 + 3] = r;
            pointData[i * 7 + 4] = g;
            pointData[i * 7 + 5] = b;
            pointData[i * 7 + 6] = a;
            onProgress(static_cast<float>(i + 1) / nPoints);
        } else {
            std::cout << "Failed to convert point data.";
            return;
        }
    }

    out.write(reinterpret_cast<char*>(&nPoints), sizeof(int64_t));
    out.write(reinterpret_cast<char*>(pointData), nFloats * sizeof(float));

    in.close();
    out.close();
}



}
}
