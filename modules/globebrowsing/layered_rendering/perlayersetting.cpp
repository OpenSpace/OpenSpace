/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/layered_rendering/perlayersetting.h>

namespace openspace {
namespace globebrowsing {

    PerLayerSetting::PerLayerSetting() {}
    PerLayerSetting::~PerLayerSetting() {}

    PerLayerFloatSetting::PerLayerFloatSetting(
        std::string name,
        std::string guiName,
        float defaultValue,
        float minimumValue,
        float maximumValue)
        : _property(name, guiName, defaultValue, minimumValue, maximumValue) { }
    PerLayerFloatSetting::~PerLayerFloatSetting(){};

    void PerLayerFloatSetting::uploadUniform(
        ProgramObject& programObject,
        GLint settingsId) {
        programObject.setUniform(settingsId, _property);
    }

    properties::Property* PerLayerFloatSetting::property() {
        return &_property;
    }

} // namespace globebrowsing
} // namespace openspace
