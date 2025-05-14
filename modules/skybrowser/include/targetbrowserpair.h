/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

    // Target & Browser
    void initialize();

    // Animation
    void startAnimation(glm::dvec3 galacticCoords, double fovEnd);
    void incrementallyAnimateToCoordinate();
    void startFading(float goal, float fadeTime);
    void stopAnimations();

    // Mouse interaction
    void startFinetuningTarget();
    void fineTuneTarget(const glm::vec2& translation);
    void synchronizeAim();

    // Browser
    void sendIdToBrowser() const;
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;

    // Target
    void centerTargetOnScreen();
    double targetRoll() const;

    bool isFacingCamera() const;
    bool isUsingRadiusAzimuthElevation() const;
    bool isEnabled() const;

    void setEnabled(bool enable);
    void setVerticalFov(double vfov);
    void setEquatorialAim(const glm::dvec2& aim);
    void setBorderColor(const glm::ivec3& color);
    void setBorderRadius(double radius);
    void setBrowserRatio(float ratio);
    void setVerticalFovWithScroll(float scroll);
    void setImageCollectionIsLoaded(bool isLoaded);
    void setPointSpaceCraft(bool shouldPoint);
    void applyRoll();

    double verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec2 targetDirectionEquatorial() const;
    glm::dvec3 targetDirectionGalactic() const;
    std::string browserGuiName() const;
    std::string browserId() const;
    std::string targetRenderableId() const;
    std::string targetNodeId() const;
    bool pointSpaceCraft() const;

    ScreenSpaceSkyBrowser* browser() const;
    std::vector<std::string> selectedImages() const;

    ghoul::Dictionary dataAsDictionary() const;

    // WorldWide Telescope image handling
    void setImageOrder(const std::string& imageUrl, int order);
    void selectImage(const ImageData& image);
    void addImageLayerToWwt(const std::string& imageUrl);
    void removeSelectedImage(const std::string& imageUrl);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(const std::string& imageUrl, float opacity);
    void hideChromeInterface();

private:
    // Target and browser
    RenderableSkyTarget* _targetRenderable = nullptr;
    ScreenSpaceSkyBrowser* _browser = nullptr;
    SceneGraphNode* _targetNode = nullptr;

    // Animation
    skybrowser::Animation<double> _fovAnimation = skybrowser::Animation(0.0, 0.0, 0.0);
    skybrowser::Animation<glm::dvec3> _targetAnimation =
        skybrowser::Animation(glm::dvec3(0.0), glm::dvec3(0.0), 0.0);
    bool _targetIsAnimating = false;
    bool _fovIsAnimating = false;

    // Dragging
    glm::dvec3 _startTargetPosition = glm::dvec3(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___TARGETBROWSERPAIR___H__
