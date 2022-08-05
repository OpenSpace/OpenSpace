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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___TARGETBROWSERPAIR___H__
#define __OPENSPACE_MODULE_SKYBROWSER___TARGETBROWSERPAIR___H__

#include <modules/skybrowser/include/utility.h>
#include <openspace/documentation/documentation.h>
#include <deque>

namespace ghoul { class Dictionary; }

namespace openspace {

struct ImageData;
class SceneGraphNode;
class ScreenSpaceSkyBrowser;
class RenderableSkyTarget;
class ScreenSpaceRenderable;

class TargetBrowserPair {
public:
    TargetBrowserPair(SceneGraphNode* target, ScreenSpaceSkyBrowser* browser);
    TargetBrowserPair& operator=(TargetBrowserPair other);

    // Target & Browser
    void initialize();

    // Animation
    void startAnimation(glm::dvec3 coordsEnd, double fovEnd);
    void incrementallyAnimateToCoordinate();
    void startFading(float goal, float fadeTime);
    void stopAnimations();

    // Mouse interaction
    void startFinetuningTarget();
    void fineTuneTarget(const glm::vec2& translation);
    void synchronizeAim();

    // Browser
    void sendIdToBrowser() const;
    void updateBrowserSize();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    bool isImageCollectionLoaded();

    // Target
    void centerTargetOnScreen();
    double targetRoll() const;

    bool isFacingCamera() const;
    bool isUsingRadiusAzimuthElevation() const;
    bool isEnabled() const;

    void setEnabled(bool enable);
    void setOpacity(float opacity);
    void setVerticalFov(double vfov);
    void setEquatorialAim(const glm::dvec2& aim);
    void setBorderColor(const glm::ivec3& color);
    void setBrowserRatio(float ratio);
    void setVerticalFovWithScroll(float scroll);
    void setImageCollectionIsLoaded(bool isLoaded);

    double verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec2 targetDirectionEquatorial() const;
    glm::dvec3 targetDirectionGalactic() const;
    std::string browserGuiName() const;
    std::string browserId() const;
    std::string targetRenderableId() const;
    std::string targetNodeId() const;
    float browserRatio() const;

    SceneGraphNode* targetNode() const;
    ScreenSpaceSkyBrowser* browser() const;
    std::vector<int> selectedImages() const;

    ghoul::Dictionary dataAsDictionary() const;

    // WorldWide Telescope image handling
    void setImageOrder(int i, int order);
    void selectImage(const ImageData& image, int i);
    void addImageLayerToWwt(const std::string& url, int i);
    void removeSelectedImage(int i);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(int i, float opacity);
    void hideChromeInterface();

    friend bool operator==(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs);
    friend bool operator!=(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs);

private:
    void aimTargetGalactic(glm::dvec3 direction);

    // Target and browser
    RenderableSkyTarget* _targetRenderable = nullptr;
    ScreenSpaceSkyBrowser* _browser = nullptr;
    SceneGraphNode* _targetNode = nullptr;

    // Animation
    skybrowser::Animation<double> _fovAnimation = skybrowser::Animation(0.0, 0.0, 0.0);
    skybrowser::Animation<glm::dvec3> _targetAnimation =
        skybrowser::Animation(glm::dvec3(0.0), glm::dvec3(0.0), 0.0);
    bool _targetIsAnimating = false;

    // Dragging
    glm::dvec3 _startTargetPosition = glm::dvec3(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___TARGETBROWSERPAIR___H__
