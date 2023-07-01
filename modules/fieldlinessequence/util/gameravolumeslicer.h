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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___GAMERAVOLUMESLICER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___GAMERAVOLUMESLICER___H__

#include <modules/fieldlinessequence/util/commons.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <string>
#include <vector>
#include <HighFive/H5File.hpp>
//#include "modules/kameleon/ext/kameleon/ext/hdf5/hdf5-1.8.12/c++/src/H5Cpp.h"
//#ifdef HAVE_HDF5 
//#include "modules/kameleon/ext/kameleon/ext/hdf5/hdf5-1.8.12/c++/src/H5Cpp.h"


namespace openspace {

class GameraVolumeSlicer {
public:
   GameraVolumeSlicer() = default;
   void getSlice(std::string pathToHdf5File, std::string axis, int value);

   std::vector<std::string> dataPropertyNames();
   std::vector<std::vector<float>> volumeDimensions();
   std::vector<std::vector<std::vector<float>>> data();

private:

    std::vector<std::vector<float>> getVolumeDimensions(HighFive::File file);
    std::vector<std::vector<std::vector<float>>> slicer(std::string axis, float value, HighFive::Group timeStep);
    void interpolator(float value, std::vector<std::vector<std::vector<float>>> slicedDataBDP,
                        std::vector<std::vector<std::vector<float>>> slicedDataADP,
                        std::vector<std::vector<std::vector<float>>>& data);

     std::vector<std::string> _dataPropertyNames;
     std::vector<std::vector<float>> _volumeDimensions; // Inner vec should be vec2
     std::vector<std::vector<std::vector<float>>> _data;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___GAMERAVOLUMESLICER___H__

