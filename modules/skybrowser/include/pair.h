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

#include <modules/skybrowser/include/wwtdatahandler.h>
//#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <openspace/documentation/documentation.h>
#include <deque>

namespace openspace {

class ScreenSpaceSkyBrowser;

class Pair {
public:

    constexpr static const float AcceptableDiff = 0.01f;

    Pair(ScreenSpaceSkyBrowser* browser, ScreenSpaceSkyTarget* target);

    void lock();
    void unlock();
    void setImageOrder(int i, int order);
    void connectPair();
    void synchronizeWithWwt();
    void removeHighlight(glm::ivec3 color);
    void highlight(glm::ivec3 color);
    void enable();
    void disable();
    
    void startAnimation(glm::dvec3 coordsEnd, float fovEnd, bool shouldLockAfter = true);
    void centerTargetOnScreen();
    void incrementallyAnimateToCoordinate(double deltaTime);
    void incrementallyFade(float goalState, float fadeTime, float deltaTime);
    bool hasFinishedFading(float goalState);
    bool isCoordOnPair(glm::vec2 mousePosition);    
    bool isEnabled();
    bool isLocked();

    glm::ivec3 borderColor();
    glm::dvec3 targetDirectionEquatorial();
    glm::dvec3 targetDirectionGalactic();
    std::string browserGuiName();
    std::string browserId();
    std::string targetId();
    float verticalFov();
    const std::deque<int>& getSelectedImages();
    void selectImage(const ImageData& image, const int i);
    void removeSelectedImage(const int i);
    void loadImages(std::string collection);
    void setImageOpacity(const int i, float opacity);
    void sendIdToBrowser();
    void updateBrowserSize();

    ScreenSpaceSkyTarget* getTarget();
    ScreenSpaceSkyBrowser* getBrowser();

    friend bool operator==(const Pair& lhs, const Pair& rhs);
    friend bool operator!=(const Pair& lhs, const Pair& rhs);

private:

    static std::string _selected;
    bool isTargetFadeFinished(float goalState);
    bool isBrowserFadeFinished(float goalState);

    ScreenSpaceSkyTarget* _target{ nullptr };
    ScreenSpaceSkyBrowser* _browser{ nullptr };
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___PAIR___H__
