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

#ifndef __OPENSPACE_MODULE_DEBUGGING___DEBUGGINGMODULE___H__
#define __OPENSPACE_MODULE_DEBUGGING___DEBUGGINGMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/font/font.h>

namespace openspace {

class DebuggingModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Debugging";

    DebuggingModule();

    std::vector<documentation::Documentation> documentations() const override;

    scripting::LuaLibrary luaLibrary() const override;

protected:
    void internalInitialize(const ghoul::Dictionary&) override;
    void internalInitializeGL() override;

private:
    properties::BoolProperty _showStatistics;
    properties::FloatProperty _statisticsScale;
    properties::BoolProperty _showFrameInformation;

    std::shared_ptr<ghoul::fontrendering::Font> _fontFrameInfo;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_DEBUGGING___DEBUGGINGMODULE___H__
