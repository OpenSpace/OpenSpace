#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <ghoul/opengl/texture.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/screenspacebrowser.h>

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

        CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
        frame->ExecuteJavaScript(script, frame->GetURL(), 0);
    }

    void ScreenSpaceSkyBrowser::translate(glm::vec3 translation) {
        glm::vec4 homogenousCoords(glm::vec4(translation, 1.0));
        glm::vec3 position = _cartesianPosition;
        _cartesianPosition = glm::translate(glm::mat4(1.f), translation) * glm::vec4(position, 1.0f);
    }
}
