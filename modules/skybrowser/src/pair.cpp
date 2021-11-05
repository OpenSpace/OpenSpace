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

#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/include/pair.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/utility.h>

namespace openspace {

    std::string Pair::_selected = ""; // Define the static variable in the global scope

    Pair::Pair(ScreenSpaceSkyBrowser* browser, ScreenSpaceSkyTarget* target)
       : _target(target), _browser(browser)
    {
        assert(browser != nullptr, "Sky browser is null pointer!");
        assert(target != nullptr, "Sky target is null pointer!");
    }

    Pair& Pair::operator=(Pair other)
    {
        std::swap(_target, other._target);
        std::swap(_browser, other._browser);
        return *this;
    }

    void Pair::lock() {
        _target->lock();
    }

    void Pair::unlock() {
        _target->unlock();
    }

    void Pair::setImageOrder(int i, int order) {
        _browser->setImageOrder(i, order);
    }

    void Pair::connectPair()
    {
        _browser->connectToSkyTarget();
        _target->findSkyBrowser();
    }

    void Pair::synchronizeWithWwt()
    {
        _browser->startSyncingWithWwt();
    }

    void Pair::removeHighlight(glm::ivec3 color)
    {
        _target->removeHighlight(color);
        _browser->removeHighlight(color);
    }

    void Pair::highlight(glm::ivec3 color)
    {
        _browser->highlight(color);
        _target->highlight(color);
    }

    bool Pair::isTargetFadeFinished(float goalState)
    {
        // Is fading finished?
        float targetDiff = abs(_target->opacity() - goalState);
        
        return targetDiff < AcceptableDiff;
    }

    bool Pair::isBrowserFadeFinished(float goalState)
    {
        float browserDiff = abs(_browser->getOpacity().value() - goalState);
        return browserDiff < AcceptableDiff;
    }

    bool Pair::isCoordOnPair(glm::vec2 mousePosition)
    {
        bool onBrowser = _browser->coordIsInsideCornersScreenSpace(mousePosition);
        bool onTarget = _target->coordIsInsideCornersScreenSpace(mousePosition);
        if (onBrowser) {
            _selected = _browser->identifier();
        }
        else if (onTarget) {
            _selected = _target->identifier();
        }

        return onBrowser || onTarget;
    }

    void Pair::enable()
    {
        _browser->property("Enabled")->set(true);
        _target->property("Enabled")->set(true);
    }

    void Pair::disable()
    {
        _browser->property("Enabled")->set(false);
        _target->property("Enabled")->set(false);
    }

    bool Pair::isEnabled()
    {
        return _target->isEnabled() && _browser->isEnabled();
    }

    bool Pair::isLocked()
    {
        return _target->isLocked();
    }

    glm::ivec3 Pair::borderColor()
    {
        return _browser->borderColor();
    }

    glm::dvec3 Pair::targetDirectionEquatorial()
    {
        return _target->directionEquatorial();
    }

    glm::dvec3 Pair::targetDirectionGalactic()
    {
        return _target->directionGalactic();
    }

    std::string Pair::browserGuiName()
    {
        return _browser->guiName();
    }

    std::string Pair::browserId()
    {
        return _browser->identifier();
    }

    std::string Pair::targetId()
    {
        return _target->identifier();
    }

    float Pair::verticalFov()
    {
        return _browser->verticalFov();
    }

    const std::deque<int>& Pair::getSelectedImages()
    {
        return _browser->getSelectedImages();
    }

    void Pair::selectImage(const ImageData& image, int i)
    {
        // Load image into browser
        _browser->addSelectedImage(image, i);

        // If the image has coordinates, move the target
        if (image.hasCelestialCoords) {

            // Animate the target to the image coordinate position
            unlock();
            startAnimation(image.equatorialCartesian, image.fov);
        }
    }

    void Pair::removeSelectedImage(const int i)
    {
        _browser->removeSelectedImage(i);
    }

    void Pair::loadImages(std::string collection)
    {
        _browser->sendMessageToWwt(wwtmessage::loadCollection(collection));
        _browser->setHasLoadedImages(true);
    }

    void Pair::setImageOpacity(const int i, float opacity)
    {
        ghoul::Dictionary msg = wwtmessage::setImageOpacity(std::to_string(i), opacity);
        _browser->sendMessageToWwt(msg);
    }

    void Pair::sendIdToBrowser()
    {
        _browser->sendIdToBrowser();
    }

    void Pair::incrementallyAnimateToCoordinate(double deltaTime) 
    {
        if (_target->isAnimated()) {
            _target->animateToCoordinate(deltaTime);
        }
        else if (_browser->isAnimated()) {
            _browser->incrementallyAnimateToFov(deltaTime);
        }
    }

    void Pair::startAnimation(glm::dvec3 coordsEnd, float FOVEnd, bool lockAfter)
    {
        _target->startAnimation(coordsEnd, lockAfter);
        _browser->startFovAnimation(FOVEnd);
    }

    void Pair::centerTargetOnScreen()
    {
        // Animate the target to the center of the screen
        unlock();
        // Get camera direction in celestial spherical coordinates
        glm::dvec3 viewDirection = skybrowser::cameraDirectionEquatorial();
        // Keep the current fov
        float currentFov = verticalFov();
        startAnimation(viewDirection, currentFov, false);
    }

    bool Pair::isFinishedFading(float goalState)
    {
        return isTargetFadeFinished(goalState) && isBrowserFadeFinished(goalState);
    }

    ScreenSpaceSkyTarget* Pair::getTarget() {
        return _target;
    }

    ScreenSpaceSkyBrowser* Pair::getBrowser() {
        return _browser;
    }

    void Pair::incrementallyFade(float goalState, float fadeTime, float deltaTime)
    {
        float opacityDelta = static_cast<float>(deltaTime / fadeTime);
    
        if (!isTargetFadeFinished(goalState)) {
            _target->setOpacity(_target->opacity() + opacityDelta);
        }
        else {
            _target->setOpacity(goalState);
        }
        
        if (!isBrowserFadeFinished(goalState)) {
            _browser->getOpacity() = _browser->getOpacity().value() + opacityDelta;
        }
        else {
            _browser->getOpacity() = goalState;
        }
    }

    bool operator==(const Pair& lhs, const Pair& rhs) {
        return lhs._target == rhs._target && lhs._browser == rhs._browser;
    }
    bool operator!=(const Pair & lhs, const Pair & rhs) {
        return !(lhs == rhs);
    }
  
} // namespace openspace
