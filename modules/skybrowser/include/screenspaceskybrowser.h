#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <modules/webbrowser/include/screenspacebrowser.h>

namespace openspace {
    class ScreenSpaceSkyTarget;

    class ScreenSpaceSkyBrowser : public ScreenSpaceBrowser
    {
    public:
        ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyBrowser() = default;

        bool deinitializeGL() override;

        // Communication with the webpage and WWT
        void executeJavascript(std::string& script) const;
        ghoul::Dictionary createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov, const bool moveInstantly = true) const;
        ghoul::Dictionary createMessageForPausingWWTTime() const;
        ghoul::Dictionary createMessageForLoadingWWTImgColl(const std::string& url) const;
        bool sendMessageToWWT(const ghoul::Dictionary& msg);
        void sendMouseEvent(CefStructBase<CefMouseEventTraits> event, int x, int y) const;
        void WWTfollowCamera();
        glm::dvec2 convertGalacticToCelestial(glm::dvec3 coords) const;
        float fieldOfView() const;
        void scrollZoom(float scroll);

        // Translation
        void translate(glm::vec2 translation);
        void translate(glm::vec2 translation, glm::vec2 position);
        // Position and dimension and corners
        glm::vec2 getScreenSpacePosition();
        glm::vec2 getScreenSpaceDimensions();
        glm::vec2 getUpperRightCornerScreenSpace();
        glm::vec2 getLowerLeftCornerScreenSpace();
        glm::vec2 getScreenSpaceBrowserDimension();
       
        // Mouse interaction
        bool coordIsInsideCornersScreenSpace(glm::vec2 coord);
        glm::vec2 coordIsOnResizeArea(glm::vec2 coord);
        // Scaling
        void scale(glm::vec2 scalingFactor);
        void scale(float scalingFactor);
        glm::mat4 scaleMatrix() override;
        // Resizing
        void saveResizeStartSize();
        void updateBrowserSize();
        // Flag for dimensions
        bool _browserDimIsDirty;
        properties::FloatProperty _fieldOfView;
        ScreenSpaceSkyTarget* _skyTarget;
    private:
        glm::vec2 _startSize;
        float _startScale;
        properties::Vec2Property _browserDimensions;
        bool _camIsSyncedWWT;
        
        std::thread _threadWWTMessages;
       
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

