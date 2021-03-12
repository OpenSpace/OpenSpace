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

        glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
        windowRatio /= windowRatio.y;

        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation * windowRatio, 0.0f)) * glm::vec4(position * windowRatio, _cartesianPosition.value().z, 1.0f);
    }

    glm::vec2  ScreenSpaceSkyBrowser::getScreenSpacePosition() {
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() *
            localRotationMatrix() * scaleMatrix();
        glm::mat4 viewProj = global::renderEngine->scene()->camera()->viewProjectionMatrix();
        glm::mat4 screenSpaceTransform = viewProj * modelTransform;

        glm::vec3 scale;
        glm::quat rotation;
        glm::vec3 translation;
        glm::vec3 skew;
        glm::vec4 perspective;
        glm::decompose(screenSpaceTransform, scale, rotation, translation, skew, perspective);

        return translation;
    }

    glm::vec2  ScreenSpaceSkyBrowser::getScreenSpaceDimensions() {
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() *
            localRotationMatrix() * scaleMatrix();
        glm::mat4 viewProj = global::renderEngine->scene()->camera()->viewProjectionMatrix();
        glm::mat4 screenSpaceTransform = viewProj * modelTransform;


        glm::vec3 scale;
        glm::quat rotation;
        glm::vec3 translation;
        glm::vec3 skew;
        glm::vec4 perspective;
        glm::decompose(screenSpaceTransform, scale, rotation, translation, skew, perspective);

        // Scale is negative and relative to the whole screen
        // Changing to positive and View Coordinates [-1,1] 
        // E.g. a full screen screenspacebrowser will have [2,2]
        scale = -2.0f * scale;

        return scale;
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

}
