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

#include <modules/skybrowser/include/targetbrowserpair.h>

#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/constants.hpp>
#include <functional>

namespace openspace {

    TargetBrowserPair::TargetBrowserPair(ScreenSpaceSkyBrowser* browser, ScreenSpaceSkyTarget* target)
       : _target(target), _browser(browser)
    {
        ghoul_assert(browser != nullptr, "Sky browser is null pointer!");
        ghoul_assert(target != nullptr, "Sky target is null pointer!");
    }

    TargetBrowserPair& TargetBrowserPair::operator=(TargetBrowserPair other)
    {
        std::swap(_target, other._target);
        std::swap(_browser, other._browser);
        return *this;
        
    }
    
    void TargetBrowserPair::lock() {
        _target->setLock(true);
    }

    void TargetBrowserPair::unlock() {
        _target->setLock(false);
    }

    void TargetBrowserPair::setImageOrder(int i, int order) {
        _browser->setImageOrder(i, order);
    }

    void TargetBrowserPair::removeHighlight(glm::ivec3 color)
    {
        _target->removeHighlight(color);
        _browser->removeHighlight(color);
    }

    void TargetBrowserPair::highlight(glm::ivec3 color)
    {
        _browser->highlight(color);
        _target->highlight(color);
    }

    bool TargetBrowserPair::isTargetFadeFinished(float goalState) const
    {
        // Is fading finished?
        float targetDiff = abs(_target->opacity() - goalState);
        
        return targetDiff < FadeThreshold;
    }

    bool TargetBrowserPair::isBrowserFadeFinished(float goalState) const
    {
        float browserDiff = abs(_browser->opacity() - goalState);
        return browserDiff < FadeThreshold;
    }

    bool TargetBrowserPair::checkMouseIntersection(glm::vec2 mousePosition)
    {
        bool onBrowser = _browser->intersection(mousePosition);
        bool onTarget = _target->intersection(mousePosition);
        if (onBrowser) {
            _selected = _browser;
            _isSelectedBrowser = true;
        }
        else if (onTarget) {
            _selected = _target;
            _isSelectedBrowser = false;
        }
        else {
            _selected = nullptr;
            _isSelectedBrowser = false;
        }
        return onBrowser || onTarget;
    }

    glm::vec2 TargetBrowserPair::selectedScreenSpacePosition()
    {
        return _selected->screenSpacePosition();
    }

    bool TargetBrowserPair::isBrowserSelected()
    {
        return _isSelectedBrowser;
    }

    bool TargetBrowserPair::isTargetSelected()
    {
        return _selected && !_isSelectedBrowser;
    }

    void TargetBrowserPair::fineTuneTarget(const glm::vec2& start, 
                                           const glm::vec2& translation)
    {
        glm::vec2 fineTune = -_browser->fineTuneVector(translation);

        openspace::global::scriptEngine->queueScript(
            "openspace.skybrowser.translateScreenSpaceRenderable(\"" + targetId() + "\","
            + std::to_string(start.x) + "," + std::to_string(start.y) + ","
            + std::to_string(translation.x) + "," + std::to_string(translation.y) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void TargetBrowserPair::translateSelected(const glm::vec2& start, 
                                              const glm::vec2& translation)
    {
        if (this && _selected) {
            std::string id = _selected->identifier();
            openspace::global::scriptEngine->queueScript(
                "openspace.skybrowser.translateScreenSpaceRenderable(\"" + id + "\","
                + std::to_string(start.x) + "," + std::to_string(start.y) + ","
                + std::to_string(translation.x) + "," + std::to_string(translation.y) + ")",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    void TargetBrowserPair::synchronizeAim()
    {
        if (!_target->isAnimated()) {
            glm::dvec2 aim;
            // To remove the lag effect when moving the camera while having a locked
            // target, send the locked coordinates to wwt
            if (_target->isLocked()) {
                aim = _target->lockedCoordinates();
            }
            else {
                aim = _target->equatorialAim();
            }
            
            _browser->setEquatorialAim(aim);
            _target->setScaleFromVfov(_browser->verticalFov());
        }
    }

    void TargetBrowserPair::setEnabled(bool enable)
    {
        _browser->setEnabled(enable);
        _target->setEnabled(enable);
    }

    bool TargetBrowserPair::isEnabled() const
    {
        return _target->isEnabled() && _browser->isEnabled();
    }

    bool TargetBrowserPair::isLocked() const
    {
        return _target->isLocked();
    }

    void TargetBrowserPair::initialize()
    {
        _target->setColor(_browser->borderColor());
        _target->setDimensions(_browser->browserPixelDimensions());
        _target->setScaleFromVfov(_browser->verticalFov());
        _browser->updateBorderColor();
    }

    glm::ivec3 TargetBrowserPair::borderColor() const
    {
        return _browser->borderColor();
    }

    glm::dvec2 TargetBrowserPair::targetDirectionEquatorial() const
    {
        return _target->equatorialAim();
    }

    glm::dvec3 TargetBrowserPair::targetDirectionGalactic() const
    {
        return _target->directionGalactic();
    }

    std::string TargetBrowserPair::browserGuiName() const
    {
        return _browser->guiName();
    }

    std::string TargetBrowserPair::browserId() const
    {
        return _browser->identifier();
    }

    std::string TargetBrowserPair::targetId() const
    {
        return _target->identifier();
    }

    std::string TargetBrowserPair::selectedId()
    {
        return _selected->identifier();
    }

    glm::vec2 TargetBrowserPair::size() const
    {
        return _browser->size();
    }

    float TargetBrowserPair::verticalFov() const
    {
        return _browser->verticalFov();
    }

    const std::deque<int>& TargetBrowserPair::getSelectedImages() const
    {
        return _browser->getSelectedImages();
    }

    void TargetBrowserPair::selectImage(const ImageData& image, int i)
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

    void TargetBrowserPair::removeSelectedImage(int i)
    {
        _browser->removeSelectedImage(i);
    }

    void TargetBrowserPair::loadImageCollection(const std::string& collection)
    {
        _browser->loadImageCollection(collection);
    }

    void TargetBrowserPair::setImageOpacity(int i, float opacity)
    {
        _browser->setImageOpacity(i, opacity);
    }

    void TargetBrowserPair::hideChromeInterface(bool shouldHide)
    {
        _browser->hideChromeInterface(shouldHide);
    }

    void TargetBrowserPair::sendIdToBrowser()
    {
        _browser->setIdInBrowser();
    }

    void TargetBrowserPair::updateBrowserSize() {
        _browser->updateBrowserSize();
    }

    void TargetBrowserPair::setIsSyncedWithWwt(bool isSynced)
    {
        _browser->setIsSyncedWithWwt(isSynced);
    }

    void TargetBrowserPair::setVerticalFov(float vfov)
    {
        _verticalFov = vfov;
        _browser->setVerticalFov(vfov);
        _target->setScaleFromVfov(vfov);
    }

    void TargetBrowserPair::setEquatorialAim(const glm::dvec2& aim)
    {
        _equatorialAim = aim;
        _target->setEquatorialAim(aim);
        _browser->setEquatorialAim(aim);
    }

    void TargetBrowserPair::setBorderColor(const glm::ivec3& color)
    {
        _borderColor = color;
        _target->setColor(color);
        _browser->setBorderColor(color);
    }

    void TargetBrowserPair::setScreenSpaceSize(const glm::vec2& dimensions)
    {
        _dimensions = dimensions;
        _target->setDimensions(dimensions);
        _browser->setScreenSpaceSize(dimensions);
    }

    void TargetBrowserPair::setVerticalFovWithScroll(float scroll)
    {
        _browser->setVerticalFovWithScroll(scroll);
    }

    void TargetBrowserPair::setSelectedWithId(const std::string& id)
    {
        if (browserId() == id) {
            _isSelectedBrowser = true;
            _selected = _browser;
        }
        else if (targetId() == id) {
            _isSelectedBrowser = false;
            _selected = _target;
        }
        else {
            _isSelectedBrowser = false;
            _selected = nullptr;
        }
    }

    void TargetBrowserPair::incrementallyAnimateToCoordinate(double deltaTime) 
    {
        // Animate the target before the field of view starts to animate
        if (_target->isAnimated()) {
            _target->incrementallyAnimateToCoordinate(static_cast<float>(deltaTime));
        }
        else if (_browser->isAnimated()) {
            _browser->incrementallyAnimateToFov(static_cast<float>(deltaTime));
        }
    }

    void TargetBrowserPair::startAnimation(glm::dvec3 equatorialCoords, float fovEnd, 
                              bool shouldLockAfter)
    {
        _target->startAnimation(equatorialCoords, shouldLockAfter);
        _browser->startFovAnimation(fovEnd);
    }

    void TargetBrowserPair::centerTargetOnScreen()
    {
        // Animate the target to the center of the screen
        unlock();
        // Get camera direction in celestial spherical coordinates
        glm::dvec3 viewDirection = skybrowser::cameraDirectionEquatorial();
        // Keep the current fov
        float currentFov = verticalFov();
        startAnimation(viewDirection, currentFov, false);
    }

    bool TargetBrowserPair::hasFinishedFading(float goalState) const
    {
        return isTargetFadeFinished(goalState) && isBrowserFadeFinished(goalState);
    }

    bool TargetBrowserPair::isFacingCamera() const
    {
        return _browser->isFacingCamera() || _target->isFacingCamera();
    }

    bool TargetBrowserPair::isUsingRadiusAzimuthElevation() const
    {
        return _browser->isUsingRaeCoords() || _target->isUsingRaeCoords();
    }

    ScreenSpaceSkyTarget* TargetBrowserPair::getTarget() {
        return _target;
    }

    ScreenSpaceSkyBrowser* TargetBrowserPair::getBrowser() {
        return _browser;
    }

    void TargetBrowserPair::incrementallyFade(float goalState, float fadeTime, float deltaTime)
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
    
    bool operator==(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs) {
        return lhs._target == rhs._target && lhs._browser == rhs._browser;
    }
    bool operator!=(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs) {
        return !(lhs == rhs);
    }
    
  
} // namespace openspace
