/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__


#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>

namespace openspace {

class ScreenSpaceSkyBrowser;

class SkyBrowserModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SkyBrowser";

    SkyBrowserModule();
    virtual ~SkyBrowserModule() = default;

    float zoomFactor() const;
    glm::dvec2 convertGalacticToCelestial(glm::dvec3 coords) const;

    void WWTfollowCamera();
    void showTarget() const;

    ghoul::Dictionary createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov, const bool moveInstantly = true) const;
    ghoul::Dictionary createMessageForPausingWWTTime() const;
    ghoul::Dictionary createMessageForLoadingWWTImgColl(const std::string& url) const;

    bool sendMessageToWWT(const ghoul::Dictionary& msg);

    void initializeBrowser(ScreenSpaceSkyBrowser* skyBrowser_);
    ScreenSpaceSkyBrowser* skyBrowser();
    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected:
    void internalInitialize(const ghoul::Dictionary& dict) override;

    properties::StringProperty _testProperty;
    properties::FloatProperty _zoomFactor;
    ScreenSpaceSkyBrowser* _skyBrowser;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
