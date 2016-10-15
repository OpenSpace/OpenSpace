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

#ifndef __PER_LAYER_SETTING_H__
#define __PER_LAYER_SETTING_H__

#include <string>

#include <ghoul/opengl/programobject.h>

#include <openspace/properties/scalarproperty.h>

namespace openspace {

    using namespace ghoul::opengl;

    class PerLayerSetting {
    public:
        PerLayerSetting();
        ~PerLayerSetting();

        virtual void uploadUniform(
            ProgramObject* programObject,
            GLint settingsId) = 0;
        virtual properties::Property* property() = 0;
    private:
    };

    class PerLayerFloatSetting : public PerLayerSetting {
    public:
        PerLayerFloatSetting(
            std::string name,
            std::string guiName,
            float defaultValue,
            float minimumValue,
            float maximumValue);
        ~PerLayerFloatSetting();
        virtual void uploadUniform(ProgramObject* programObject, GLint settingsId);
        virtual properties::Property* property();
    private:
        properties::FloatProperty _property;
    };

} // namespace openspace
#endif  // __PER_LAYER_SETTING_H__
