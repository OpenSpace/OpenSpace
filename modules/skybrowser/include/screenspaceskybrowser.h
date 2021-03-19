#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <modules/webbrowser/include/screenspacebrowser.h>

namespace openspace {

    class ScreenSpaceSkyBrowser : public ScreenSpaceBrowser
    {
    public:
        ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyBrowser() = default;
        void executeJavascript(std::string& script) const;
        void translate(glm::vec2 translation);
        void translate(glm::vec2 translation, glm::vec2 position);
        glm::vec2 getScreenSpacePosition();
        glm::vec2 getScreenSpaceDimensions();
        void sendMouseEvent(CefStructBase<CefMouseEventTraits> event, int x, int y) const;
        glm::vec2 getUpperRightCornerScreenSpace();
        glm::vec2 getLowerLeftCornerScreenSpace();
        bool coordIsInsideCornersScreenSpace(glm::vec2 coord);
<<<<<<< Updated upstream
        glm::vec2 coordIsOnResizeArea(glm::vec2 coord);

=======
        bool coordIsOnResizeButton(glm::vec2 coord);
     
>>>>>>> Stashed changes
        void scale(glm::vec2 scalingFactor);
        glm::mat4 scaleMatrix() override;
        void saveResizeStartSize();
        void updateBrowserSize();
        void scale(float scalingFactor);

        glm::vec2 getScreenSpaceBrowserDimension();
        bool _browserDimIsDirty;
    private:
        glm::vec2 _startSize;
        float _startScale;
        properties::Vec2Property _browserDimensions;
       
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

