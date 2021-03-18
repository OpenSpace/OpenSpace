#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <ghoul/opengl/texture.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/windowdelegate.h>
#include <glm/gtx/string_cast.hpp>

#include <openspace/rendering/renderengine.h>
#include <openspace/util/camera.h>
#include <openspace/scene/scene.h>
#include <glm/gtx/matrix_decompose.hpp>
#include <glm/gtx/string_cast.hpp>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyBrowser";

} // namespace

namespace openspace {
    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary) 
        : ScreenSpaceBrowser(dictionary)
    {
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyBrowser";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);
        // The projection plane seems to be located at z = -2.1 so at that place the ScreenSpaceRenderables behaves like
        // they are in screen space
        _cartesianPosition.setValue(glm::vec3(_cartesianPosition.value().x, _cartesianPosition.value().y, -2.1f));
    }

    void ScreenSpaceSkyBrowser::executeJavascript(std::string& script) const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetMainFrame()) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }      
    }

    void ScreenSpaceSkyBrowser::sendMouseEvent(CefStructBase<CefMouseEventTraits> event, int x, int y)  const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        LINFO(std::to_string(_objectSize.x) + "  " + std::to_string(_objectSize.y));
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetHost()) {

            //_browserInstance->getBrowser()->GetHost()->SendMouseWheelEvent(event, x, y);
            //LINFOC(_loggerCat, "Sending scroll");

        }
    }

    void ScreenSpaceSkyBrowser::translate(glm::vec2 translation) {
        glm::vec3 position = _cartesianPosition;     
        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation, 0.0f)) * glm::vec4(position, 1.0f);
    }

    void ScreenSpaceSkyBrowser::translate(glm::vec2 translation, glm::vec2 position) {
        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation, 0.0f)) * glm::vec4(position, _cartesianPosition.value().z, 1.0f);
    }

    glm::vec2  ScreenSpaceSkyBrowser::getScreenSpacePosition() {
       return glm::vec2(_cartesianPosition.value().x, _cartesianPosition.value().y);
    }

    glm::vec2  ScreenSpaceSkyBrowser::getScreenSpaceDimensions() {
         return glm::vec2(2.f*_scale* static_cast<float>(_objectSize.x) / static_cast<float>(_objectSize.y), 2.f*_scale);
    }

    glm::vec2 ScreenSpaceSkyBrowser::getUpperRightCornerScreenSpace() {
        return getScreenSpacePosition() + (getScreenSpaceDimensions()/2.0f);
    }

    glm::vec2 ScreenSpaceSkyBrowser::getLowerLeftCornerScreenSpace() {
        return getScreenSpacePosition() - (getScreenSpaceDimensions()/2.0f);
    }

    bool ScreenSpaceSkyBrowser::coordIsInsideCornersScreenSpace(glm::vec2 coord) {
        bool lessThanUpperRight = coord.x < getUpperRightCornerScreenSpace().x && coord.y < getUpperRightCornerScreenSpace().y;
        bool moreThanLowerLeft = coord.x > getLowerLeftCornerScreenSpace().x && coord.y > getLowerLeftCornerScreenSpace().y;
        return  lessThanUpperRight && moreThanLowerLeft;
    }

    glm::vec2 ScreenSpaceSkyBrowser::coordIsOnResizeArea(glm::vec2 coord) {
        glm::vec2 resizePosition = glm::vec2{ 0 };
        // Make sure coord is on browser
        if (!coordIsInsideCornersScreenSpace(coord)) return resizePosition;

        float resizeButtonSize = 0.1f;
       
        bool isOnTop = coord.y > getUpperRightCornerScreenSpace().y - (getScreenSpaceDimensions().y * resizeButtonSize);
        bool isOnBottom = coord.y < getLowerLeftCornerScreenSpace().y + (getScreenSpaceDimensions().y * resizeButtonSize);
        bool isOnRight = coord.x > getUpperRightCornerScreenSpace().x - (getScreenSpaceDimensions().x * resizeButtonSize);
        bool isOnLeft = coord.x < getLowerLeftCornerScreenSpace().x + (getScreenSpaceDimensions().x * resizeButtonSize);

        resizePosition.x = isOnRight ? 1.f : isOnLeft ? -1.f : 0.f;
        resizePosition.y = isOnTop ? 1.f : isOnBottom ? -1.f : 0.f;

        return  resizePosition;
    }
    // Scales the ScreenSpaceBrowser to a new ratio
    void ScreenSpaceSkyBrowser::scale(glm::vec2 scalingFactor) {       
       
        // Scale on the y axis, this is to ensure that _scale = 1 is
        // equal to the height of the window
        scale(abs(scalingFactor.y)); 
        // Resize the dimensions of the texture on the x axis
        glm::vec2 newSize = abs(scalingFactor) * _startSize;
        _texture->setDimensions(glm::ivec3(newSize, 1));
        // To not make it glitch... Makes it glitch in other ways however
        //updateBrowserSize();
    }

    glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale tells us how much of the windows height the
        // browser covers: eg a browser that covers 0.25 of the 
        // height of the window will have scale = 0.25
        float textureRatio =
            static_cast<float>(_texture->dimensions().x) / static_cast<float>(_texture->dimensions().y);

        glm::mat4 scale = glm::scale(
            glm::mat4(1.f),
            glm::vec3(textureRatio * _scale, _scale, 1.f)
        );
        return scale;
    }

    void ScreenSpaceSkyBrowser::saveResizeStartSize() {
        _startSize = glm::vec2(_dimensions.value().x, _dimensions.value().y);
        _startScale = _scale.value();
    }
    // Updates the browser size to match the size of the texture
    void ScreenSpaceSkyBrowser::updateBrowserSize() {
        _dimensions = _texture->dimensions();
    }
    void ScreenSpaceSkyBrowser::scale(float scalingFactor) {
        _scale = _startScale * scalingFactor;
    }
}
