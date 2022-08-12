/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <modules/skybrowser/include/wwtcommunicator.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>

namespace openspace {

class ScreenSpaceSkyBrowser : public ScreenSpaceRenderable, public WwtCommunicator {
public:
    explicit ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceSkyBrowser() override;

    bool initializeGL() override;
    bool deinitializeGL() override;
    glm::mat4 scaleMatrix() override;
    void render() override;
    void update() override;

    float opacity() const;
    glm::dvec2 fineTuneVector(const glm::dvec2& drag);
    bool isInitialized() const;

    void setVerticalFovWithScroll(float scroll);
    void setOpacity(float opacity);
    void setRatio(float ratio);
    void setIdInBrowser() const;
    void setIsInitialized(bool isInitialized);

    void updateTextureResolution();

    // Copies rendered
    void addDisplayCopy(const glm::vec3& raePosition, int nCopies);
    void removeDisplayCopy();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    std::vector<std::pair<std::string, bool>> showDisplayCopies() const;

    static documentation::Documentation Documentation();

private:
    properties::FloatProperty _textureQuality;
    properties::BoolProperty _isHidden;
    std::vector<std::unique_ptr<properties::Vec3Property>> _displayCopies;
    std::vector<std::unique_ptr<properties::BoolProperty>> _showDisplayCopies;

    void bindTexture() override;

    // Flags
    bool _isSyncedWithWwt = false;
    bool _textureDimensionsIsDirty = false;
    bool _ratioIsDirty = false;
    bool _radiusIsDirty = false;
    bool _isInitialized = false;

    float _ratio = 1.f;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
