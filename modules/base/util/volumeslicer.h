/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___VOLUMESLICER___H__
#define __OPENSPACE_MODULE_BASE___VOLUMESLICER___H__

#include <modules/kameleon/ext/kameleon/src/ccmc/Kameleon.h>

namespace openspace {

class VolumeSlicer {
public:
    VolumeSlicer() = default;
    VolumeSlicer(
        std::filesystem::path path,
        std::string axis,
        std::string cutValue,
        std::string dataProperty
    );
    VolumeSlicer(
        ccmc::Kameleon* kameleon,
        std::string axis,
        std::string cutValue,
        std::string dataProperty
    );
    void getSlicer();

private:
    std::vector<std::string> _dataProperyNames;
    std::string _dataPropery;
    glm::vec3 _volumeDimensions;
    std::vector<std::vector<std::vector<float>>> _data;

};
}// namespace openspace

#endif //__OPENSPACE_MODULE_BASE___VOLUMESLICER___H__
