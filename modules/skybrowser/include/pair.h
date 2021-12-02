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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___PAIR___H__
#define __OPENSPACE_MODULE_SKYBROWSER___PAIR___H__

#include <openspace/documentation/documentation.h>
#include <deque>

namespace openspace {

class ScreenSpaceSkyBrowser;
class ScreenSpaceSkyTarget;
class ScreenSpaceRenderable;
class ImageData;

class Pair {
public:

    constexpr static const float FadeThreshold = 0.01f;
    constexpr static const double AnimationThreshold = 0.0001f;

    Pair(ScreenSpaceSkyBrowser* browser, ScreenSpaceSkyTarget* target);
    Pair(Pair const&) = default;
    // user-defined copy assignment (copy-and-swap idiom)
    Pair& operator=(Pair other);

    // Target & Browser
    void initialize();
    // Highlighting
    void removeHighlight(glm::ivec3 color); 
    void highlight(glm::ivec3 color);
    // Animation
    void startAnimation(glm::dvec3 coordsEnd, float fovEnd, bool shouldLockAfter = true);
    void incrementallyAnimateToCoordinate(double deltaTime);
    void incrementallyFade(float goalState, float fadeTime, float deltaTime);
    // Mouse interaction
    bool checkMouseIntersection(glm::vec2 mousePosition);
    glm::vec2 selectedScreenSpacePosition();
    bool isSelectedBrowser();

    // Browser
    void sendIdToBrowser();
    void updateBrowserSize();

    // Target
    void centerTargetOnScreen();
    void lock();
    void unlock();

    // Boolean functions
    bool hasFinishedFading(float goalState) const;
    
    bool isEnabled() const;
    bool isLocked() const;

    // Setters
    void setEnabled(bool enable);
    void setIsSyncedWithWwt(bool isSynced);

    // Getters by value 
    float verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec3 targetDirectionEquatorial() const;
    glm::dvec3 targetDirectionGalactic() const;
    std::string browserGuiName() const;
    std::string browserId() const;
    std::string targetId() const;

    // Getters by reference
    ScreenSpaceSkyTarget* getTarget();
    ScreenSpaceSkyBrowser* getBrowser();
    const std::deque<int>& getSelectedImages() const;
    
    // WorldWide Telescope image handling
    void setImageOrder(int i, int order);
    void selectImage(const ImageData& image, int i);
    void removeSelectedImage(int i);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(int i, float opacity);

    // Comparision operators
    friend bool operator==(const Pair& lhs, 
                           const Pair& rhs);
    friend bool operator!=(const Pair& lhs, 
                           const Pair& rhs);

private:
    ScreenSpaceRenderable* _selected;
    bool _isSelectedBrowser;

    bool isTargetFadeFinished(float goalState) const;
    bool isBrowserFadeFinished(float goalState) const;

    ScreenSpaceSkyTarget* _target{ nullptr };
    ScreenSpaceSkyBrowser* _browser{ nullptr };
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___PAIR___H__
