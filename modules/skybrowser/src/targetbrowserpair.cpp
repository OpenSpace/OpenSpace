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

#include <modules/skybrowser/include/targetbrowserpair.h>

#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/renderableskytarget.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/constants.hpp>
#include <functional>

namespace openspace {

TargetBrowserPair::TargetBrowserPair(SceneGraphNode* targetNode, 
                                        ScreenSpaceSkyBrowser* browser)
    : _targetNode(targetNode), _browser(browser)
{
    ghoul_assert(browser != nullptr, "Sky browser is null pointer!");
    ghoul_assert(target != nullptr, "Sky target is null pointer!");

    _targetRenderable = dynamic_cast<RenderableSkyTarget*>(_targetNode->renderable());
}

TargetBrowserPair& TargetBrowserPair::operator=(TargetBrowserPair other) {
    std::swap(_targetNode, other._targetNode);
    std::swap(_browser, other._browser);
    return *this;
}

void TargetBrowserPair::setImageOrder(int i, int order) {
    _browser->setImageOrder(i, order);
}

void TargetBrowserPair::removeHighlight(const glm::ivec3& color) {
    _targetRenderable->removeHighlight(color);
    _browser->removeHighlight(color);
}

void TargetBrowserPair::highlight(const glm::ivec3& color) {
    _browser->highlight(color);
    _targetRenderable->highlight(color);
}

void TargetBrowserPair::aimTargetGalactic(glm::dvec3 direction) {
    std::string id = _targetNode->identifier();
    // Uris for properties
    std::string positionUri = "Scene." + id + ".Translation.Position";
    glm::dvec3 positionCelestial = glm::normalize(direction) * 
        skybrowser::CelestialSphereRadius;
    std::string setValue = "openspace.setPropertyValueSingle('";

    openspace::global::scriptEngine->queueScript(
        setValue + positionUri + "', " + ghoul::to_string(positionCelestial) + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

bool TargetBrowserPair::checkMouseIntersection(const glm::vec2& mousePosition) {
    _selected = _browser->isIntersecting(mousePosition) ? _browser : nullptr;

    return _selected;
}

glm::vec2 TargetBrowserPair::selectedScreenSpacePosition() const {
    return _selected->screenSpacePosition();
}

// The fine tune of the target is a way to "drag and drop" the target with right click 
// drag on the sky browser window. This is to be able to drag the target around when it 
// has a very small field of view
void TargetBrowserPair::fineTuneTarget(const glm::dvec3& startWorldPosition, 
                                       const glm::vec2& startMouse,
                                       const glm::vec2& translation)
{
    glm::vec2 fineTune = _browser->fineTuneVector(translation);
    glm::vec2 endMouse = startMouse + fineTune;
    
    // Translation world
    glm::dvec3 startWorld = skybrowser::localCameraToGalactic(
        glm::vec3(startMouse, skybrowser::ScreenSpaceZ)
    );
    glm::dvec3 endWorld = skybrowser::localCameraToGalactic(
        glm::vec3(endMouse, skybrowser::ScreenSpaceZ)
    );

    glm::dvec3 translationWorld = endWorld - startWorld;
    aimTargetGalactic(startWorldPosition + translationWorld);
}

void TargetBrowserPair::translateSelected(const glm::vec2& start, 
                                            const glm::vec2& trans)
{
    if (this && _selected) {
        std::string id = _selected->identifier();
        openspace::global::scriptEngine->queueScript(
            "openspace.skybrowser.translateScreenSpaceRenderable(\"" + id + "\","
            + std::to_string(start.x) + "," + std::to_string(start.y) + ","
            + std::to_string(trans.x) + "," + std::to_string(trans.y) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void TargetBrowserPair::synchronizeAim() {
    if (!_targetIsAnimated) {
        // To remove the lag effect when moving the camera while having a locked
        // target, send the locked coordinates to wwt
        glm::dvec2 aim = targetDirectionEquatorial();
        _browser->setEquatorialAim(aim);
        _targetRenderable->setVerticalFov(_browser->verticalFov());
    }
}

void TargetBrowserPair::setEnabled(bool enable) {
    _browser->setEnabled(enable);
    _targetRenderable->property("Enabled")->set(false);
}

void TargetBrowserPair::setOpacity(float opacity)
{
    _browser->property("Opacity")->set(opacity);
    _targetRenderable->property("Opacity")->set(opacity);
}

bool TargetBrowserPair::isEnabled() const {
    return _targetRenderable->isEnabled() || _browser->isEnabled();
}

void TargetBrowserPair::initialize() {
    _targetRenderable->setColor(_browser->borderColor());
    _targetRenderable->setDimensions(_browser->screenSpaceDimensions());
    _browser->updateBorderColor();
}

glm::ivec3 TargetBrowserPair::borderColor() const {
    return _browser->borderColor();
}

glm::dvec2 TargetBrowserPair::targetDirectionEquatorial() const {
    glm::dvec3 cartesian = skybrowser::galacticToEquatorial(
        glm::normalize(_targetNode->worldPosition())
    );
    return skybrowser::cartesianToSpherical(cartesian);
}

glm::dvec3 TargetBrowserPair::targetDirectionGalactic() const {
    return glm::normalize(_targetNode->worldPosition());
}

std::string TargetBrowserPair::browserGuiName() const {
    return _browser->guiName();
}

std::string TargetBrowserPair::browserId() const {
    return _browser->identifier();
}

std::string TargetBrowserPair::targetRenderableId() const {
    return _targetRenderable->identifier();
}

std::string TargetBrowserPair::targetNodeId() const {
    return _targetNode->identifier();
}

std::string TargetBrowserPair::selectedId() {
    return _selected->identifier();
}

glm::vec2 TargetBrowserPair::size() const {
    return _browser->size();
}

float TargetBrowserPair::verticalFov() const {
    return _browser->verticalFov();
}

const std::deque<int>& TargetBrowserPair::selectedImages() const {
    return _browser->getSelectedImages();
}

void TargetBrowserPair::selectImage(const ImageData& image, int i) {
    // Load image into browser
    _browser->displayImage(image.imageUrl, i);

    // If the image has coordinates, move the target
    if (image.hasCelestialCoords) {

        // Animate the target to the image coordinate position
        // unlock();
        glm::dvec3 galactic = skybrowser::equatorialToGalactic(image.equatorialCartesian);
        startAnimation(galactic * skybrowser::CelestialSphereRadius, image.fov, true);
    }
}

void TargetBrowserPair::removeSelectedImage(int i) {
    _browser->removeSelectedImage(i);
}

void TargetBrowserPair::loadImageCollection(const std::string& collection) {
    _browser->loadImageCollection(collection);
}

void TargetBrowserPair::setImageOpacity(int i, float opacity) {
    _browser->setImageOpacity(i, opacity);
}

void TargetBrowserPair::hideChromeInterface(bool shouldHide) {
    _browser->hideChromeInterface(shouldHide);
}

void TargetBrowserPair::sendIdToBrowser() {
    _browser->setIdInBrowser();
}

void TargetBrowserPair::updateBrowserSize() {
    _browser->updateBrowserSize();
}

void TargetBrowserPair::setIsSyncedWithWwt(bool isSynced) {
    _browser->setIsSyncedWithWwt(isSynced);
}

void TargetBrowserPair::setVerticalFov(float vfov) {
    _browser->setVerticalFov(vfov);
    _targetRenderable->setVerticalFov(vfov);
}

void TargetBrowserPair::setEquatorialAim(const glm::dvec2& aim) {
    _equatorialAim = aim;
    aimTargetGalactic(
        skybrowser::equatorialToGalactic(skybrowser::sphericalToCartesian(aim))
    );
    _browser->setEquatorialAim(aim);
}

void TargetBrowserPair::setBorderColor(const glm::ivec3& color) {
    _borderColor = color;
    _targetRenderable->setColor(color);
    _browser->setBorderColor(color);
}

void TargetBrowserPair::setScreenSpaceSize(const glm::vec2& dimensions) {
    _browser->setScreenSpaceSize(dimensions);
    _targetRenderable->setDimensions(dimensions);
}

void TargetBrowserPair::setVerticalFovWithScroll(float scroll) {
    _browser->setVerticalFovWithScroll(scroll);
}

void TargetBrowserPair::incrementallyAnimateToCoordinate(double deltaTime) {
    // Animate the target before the field of view starts to animate
    if (_targetIsAnimated) {
        incrementallyAnimateTarget(static_cast<float>(deltaTime));
    }
    else if (_browser->isAnimated()) {
        _browser->incrementallyAnimateToFov(static_cast<float>(deltaTime));
        _targetRenderable->setVerticalFov(_browser->verticalFov());
    }
}

void TargetBrowserPair::startFading(float goal, float fadeTime)
{
    _startTarget = _targetRenderable->opacity();
    _startBrowser = _browser->opacity();
    _goal = goal;
    _fadeTime = std::chrono::milliseconds(static_cast<int>(fadeTime * 1000));
    _fadeStart = std::chrono::system_clock::now();
    _isFading = true;
}

void TargetBrowserPair::startAnimation(glm::dvec3 galacticCoords, float fovEnd, 
                            bool shouldLockAfter)
{
    _browser->startFovAnimation(fovEnd);

    // Target animation
    _animationStart = glm::normalize(_targetNode->worldPosition()) * 
        skybrowser::CelestialSphereRadius;
    _animationEnd = galacticCoords;
    _shouldLockAfterAnimation = shouldLockAfter;
    _targetIsAnimated = true;
}

void TargetBrowserPair::incrementallyAnimateTarget(float deltaTime) {
    // At fps that are too low, the animation stops working. Just place target instead
    bool fpsTooLow = deltaTime > DeltaTimeThreshold;
    // Find smallest angle between the two vectors
    double smallestAngle = skybrowser::angleBetweenVectors(glm::normalize(_animationStart),
        glm::normalize(_animationEnd));
    bool hasArrived = smallestAngle < _targetRenderable->stopAnimationThreshold();

    // Only keep animating when target is not at goal position
    if (!hasArrived && !fpsTooLow) {
        glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
            glm::normalize(_animationStart),
            glm::normalize(_animationEnd),
            deltaTime,
            _targetRenderable->animationSpeed()
        );

        // Rotate target direction
        glm::dvec3 newPos = glm::dvec3(rotMat * glm::dvec4(_animationStart, 1.0));

        aimTargetGalactic(newPos);

        // Update position
        _animationStart = newPos;
    }
    else {
        // Set the exact target position 
        aimTargetGalactic(_animationEnd);
        _targetIsAnimated = false;
    }
}

void TargetBrowserPair::centerTargetOnScreen() {
    // Get camera direction in celestial spherical coordinates
    glm::dvec3 viewDirection = skybrowser::cameraDirectionGalactic();
    // Keep the current fov
    float currentFov = verticalFov();
    startAnimation(viewDirection, currentFov, false);
}

bool TargetBrowserPair::hasFinishedFading() const {
    return !_isFading;
}

bool TargetBrowserPair::isFacingCamera() const {
    return _browser->isFacingCamera();
}

bool TargetBrowserPair::isUsingRadiusAzimuthElevation() const {
    return _browser->isUsingRaeCoords();
}

SceneGraphNode* TargetBrowserPair::targetNode() const {
    return _targetNode;
}

ScreenSpaceSkyBrowser* TargetBrowserPair::browser() const {
    return _browser;
}

void TargetBrowserPair::incrementallyFade(float deltaTime)
{
    using namespace std::chrono;
    system_clock::time_point now = system_clock::now();
    std::chrono::duration<double, std::milli> timeSpent = now - _fadeStart;

    if (timeSpent.count() > _fadeTime.count()) {
        _isFading = false;
        _browser->setOpacity(_goal);
        _targetRenderable->setOpacity(_goal);
    }
    else {
        float percentage = timeSpent / _fadeTime;
        float newOpacityTarget;
        float newOpacityBrowser;
        if (_goal > _startTarget || _goal > _startBrowser) {
            newOpacityTarget, newOpacityBrowser = _goal * percentage;
        }
        else {
            newOpacityTarget = _startTarget * (1.f - percentage);
            newOpacityBrowser = _startBrowser * (1.f - percentage);
        }
        _browser->setOpacity(newOpacityBrowser);
        _targetRenderable->setOpacity(newOpacityTarget);
    }
}
    
bool operator==(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs) {
    return lhs._targetNode == rhs._targetNode && lhs._browser == rhs._browser;
}

bool operator!=(const TargetBrowserPair& lhs, const TargetBrowserPair& rhs) {
    return !(lhs == rhs);
}
} // namespace openspace
