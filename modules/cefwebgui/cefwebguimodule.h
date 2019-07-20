/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_CEFWEBGUI___CEFWEBGUIMODULE___H__
#define __OPENSPACE_MODULE_CEFWEBGUI___CEFWEBGUIMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/webgui/webguimodule.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/stringproperty.h>

namespace openspace {

class BrowserInstance;

class CefWebGuiModule : public OpenSpaceModule {
public:
    static constexpr const char* Name = "CefWebGui";

    CefWebGuiModule();

    void internalInitialize(const ghoul::Dictionary& configuration) override;

private:
    void startOrStopGui();

    properties::BoolProperty _enabled;
    properties::BoolProperty _visible;
    properties::TriggerProperty _reload;
    properties::StringProperty _url;
    properties::FloatProperty _guiScale;
    std::unique_ptr<BrowserInstance> _instance;

    WebGuiModule::CallbackHandle _endpointCallback = -1;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_CEFWEBGUI___CEFWEBGUIMODULE___H__
