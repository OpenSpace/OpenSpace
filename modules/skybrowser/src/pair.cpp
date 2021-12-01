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

#include <modules/skybrowser/include/pair.h>

#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/include/utility.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/constants.hpp>
#include <functional>

#pragma optimize("", off)

namespace openspace {

    std::string Pair::_selected = ""; // Define the static variable in the global scope

    Pair::Pair(ScreenSpaceSkyBrowser* browser, ScreenSpaceSkyTarget* target)
       : _target(target), _browser(browser)
    {
        ghoul_assert(browser != nullptr, "Sky browser is null pointer!");
        ghoul_assert(target != nullptr, "Sky target is null pointer!");
        
        // Set browser callback functions
        // Set callback functions so that the target and the browser update each other
        _browser->setCallbackEquatorialAim(
            [&](const glm::dvec3& equatorialAim, bool) {
                double diff = glm::length(equatorialAim - _target->equatorialAim());
                if ( diff > AnimationThreshold) {
                    _target->setCartesianPosition(
                        skybrowser::equatorialToScreenSpace3d(equatorialAim)
                    );
                }
            }
        );
        _browser->setCallbackBorderColor(
            [&](const glm::ivec3& color) {
                _target->setColor(color);
            }
        );
        _browser->setCallbackVerticalFov(
            [&](float vfov) {
                _target->setScaleFromVfov(vfov);
            }
        );
        _browser->setCallbackDimensions(
            [&](const glm::vec2& dimensions) {
                _target->setDimensions(dimensions);
            }
        );
        // Always make sure that the target and browser are visible together
        _browser->setCallbackEnabled(
            [&](bool enabled) {
                _target->setEnabled(enabled);
            }
        ); 

        // Set target callback functions
        _target->setCallbackEnabled(
            [&](bool enabled) {
                _browser->setEnabled(enabled);
            }
        );
        _target->setCallbackPosition(
            [&](glm::vec3 localCameraPosition) {
                double diff = glm::length(
                    _browser->equatorialAim() - _target->equatorialAim()
                );
                if (diff > AnimationThreshold) {
                    _browser->setEquatorialAim(
                        skybrowser::localCameraToEquatorial(localCameraPosition)
                    );
                }
            }
        );
    }

    Pair& Pair::operator=(Pair other)
    {
        std::swap(_target, other._target);
        std::swap(_browser, other._browser);
        return *this;
        
    }
    
    void Pair::lock() {
        _target->setLock(true);
    }

    void Pair::unlock() {
        _target->setLock(false);
    }

    void Pair::setImageOrder(int i, int order) {
        _browser->setImageOrder(i, order);
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
        
        return targetDiff < FadeThreshold;
    }

    bool Pair::isBrowserFadeFinished(float goalState)
    {
        float browserDiff = abs(_browser->opacity() - goalState);
        return browserDiff < FadeThreshold;
    }

    bool Pair::isCoordOnPair(glm::vec2 mousePosition)
    {
        const bool onBrowser = _browser->coordIsInsideCornersScreenSpace(mousePosition);
        const bool onTarget = _target->coordIsInsideCornersScreenSpace(mousePosition);
        if (onBrowser) {
            _selected = _browser->identifier();
        }
        else if (onTarget) {
            _selected = _target->identifier();
        }

        return onBrowser || onTarget;
    }

    void Pair::setEnabled(bool enable)
    {
        _browser->setEnabled(enable);
        _target->setEnabled(enable);
    }

    bool Pair::isEnabled()
    {
        return _target->isEnabled() && _browser->isEnabled();
    }

    bool Pair::isLocked()
    {
        return _target->isLocked();
    }

    void Pair::initialize()
    {
        _target->setColor(_browser->borderColor());
        _target->setDimensions(_browser->browserPixelDimensions());
        _target->setScaleFromVfov(_browser->verticalFov());
        _browser->updateBorderColor();
    }

    glm::ivec3 Pair::borderColor()
    {
        return _browser->borderColor();
    }

    glm::dvec3 Pair::targetDirectionEquatorial()
    {
        return _target->equatorialAim();
    }

    glm::dvec3 Pair::targetDirectionGalactic()
    {
        return _target->directionGalactic();
    }

    std::string Pair::browserGuiName()
    {
        return _browser->guiName();
    }

    const std::string& Pair::browserId() const
    {
        return _browser->identifier();
    }

    const std::string& Pair::targetId() const
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
        _browser->displayImage(image.imageUrl, i);

        // If the image has coordinates, move the target
        if (image.hasCelestialCoords) {

            // Animate the target to the image coordinate position
            unlock();
            startAnimation(image.equatorialCartesian, image.fov, true);
        }
    }

    void Pair::removeSelectedImage(const int i)
    {
        _browser->removeSelectedImage(i);
    }

    void Pair::loadImageCollection(std::string collection)
    {
        _browser->loadImageCollection(collection);
    }

    void Pair::setImageOpacity(const int i, float opacity)
    {
        _browser->setImageOpacity(i, opacity);
    }

    void Pair::sendIdToBrowser()
    {
        _browser->setIdInBrowser();
    }

    void Pair::updateBrowserSize() {
        _browser->updateBrowserSize();
    }

    void Pair::setIsSyncedWithWwt(bool isSynced)
    {
        _browser->setIsSyncedWithWwt(isSynced);
    }

    void Pair::incrementallyAnimateToCoordinate(double deltaTime) 
    {
        // Animate the target before the field of view starts to animate
        if (_target->isAnimated()) {
            _target->incrementallyAnimateToCoordinate(deltaTime);
        }
        else if (_browser->isAnimated()) {
            _browser->incrementallyAnimateToFov(deltaTime);
        }
    }

    void Pair::startAnimation(glm::dvec3 equatorialCoords, float fovEnd, bool shouldLockAfter)
    {
        _target->startAnimation(equatorialCoords, shouldLockAfter);
        _browser->startFovAnimation(fovEnd);
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

    bool Pair::hasFinishedFading(float goalState)
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
        if (_target->opacity() > goalState) {
            opacityDelta *= -1.f;
        }
    
        if (!isTargetFadeFinished(goalState)) {
            _target->setOpacity(_target->opacity() + opacityDelta);
        }
        else {
            _target->setOpacity(goalState);
        }
        
        if (!isBrowserFadeFinished(goalState)) {
            _browser->setOpacity(_browser->opacity() + opacityDelta);
        }
        else {
            _browser->setOpacity(goalState);
        }
    }
    
    bool operator==(const Pair& lhs, const Pair& rhs) {
        return lhs._target == rhs._target && lhs._browser == rhs._browser;
    }
    bool operator!=(const Pair& lhs, const Pair& rhs) {
        return !(lhs == rhs);
    }
    
  
} // namespace openspace
