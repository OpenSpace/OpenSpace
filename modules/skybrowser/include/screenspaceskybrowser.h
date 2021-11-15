#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec3property.h>
#include <deque>

namespace openspace {
    //class ScreenSpaceSkyTarget;

    class ScreenSpaceSkyBrowser : public ScreenSpaceBrowser
    {
    public:
        // Constructor and destructor
        ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyBrowser();

        // Inherited functions
        bool initializeGL() override;
        bool deinitializeGL() override;
        glm::mat4 scaleMatrix() override;
        
        // Target - browser connection
        bool connectToSkyTarget();
        bool isAnimated();
        void startFovAnimation(float fov);
        void incrementallyAnimateToFov(float deltaTime);

        void startSyncingWithWwt();
        glm::dvec2 fineTuneVector(glm::dvec2 drag);
        
        // Getters returning values
        bool hasLoadedImages() const;
        glm::vec2 browserPixelDimensions() const;
        glm::ivec3 borderColor() const;
        float verticalFov() const;
        glm::dvec2 fieldsOfView();

        // Getters returning references
        ScreenSpaceSkyTarget* getSkyTarget();
        const std::deque<int>& getSelectedImages();
        properties::FloatProperty& getOpacity();
        
        // Setters
        void setHasLoadedImages(bool isLoaded);
        void setVerticalFov(float vfov);
        void setVerticalFovWithScroll(float scroll);
        void setScale(glm::vec2 scalingFactor);
        void setScale(float scalingFactor);
        void setWebpageBorderColor(glm::ivec3 color);   

        void sendIdToBrowser();

        // Display
        void highlight(glm::ivec3 addition);
        void removeHighlight(glm::ivec3 removal);

        // Communication with WorldWide Telescope
        void addSelectedImage(const std::string& url, int i);
        void removeSelectedImage(int i);
        void setImageOrder(int i, int order);
        void sendMessageToWwt(const ghoul::Dictionary& msg);
        void syncWwtView();

        // Mouse interaction with the browser. Returns 1 or -1 at the coordinate in 
        // image if the mouse is on a side of the browser
        //            __1__
        //   y|   -1 |_____|1 
        //    |__x     -1  
        glm::vec2 isOnResizeArea(glm::vec2 screenSpaceCoord);

        // Resize functions
        void saveResizeStartSize();
        void updateBrowserSize();

        // Translation
        //void translate(glm::vec2 translation);
        
    private:
        // Communication with the web page
        void executeJavascript(std::string script);

        // Properties
        properties::FloatProperty _verticalFov;       
        properties::StringProperty _skyTargetId;
        properties::Vec2Property _browserDimensions;
        properties::IVec3Property _borderColor;

        // Flags
        bool _hasLoadedImages{ false };
        bool _isSyncedWithWwt{ false };
        bool _isFovAnimated{ false };
        float _endVfov{ 0.f };
        float _fovDiff{ 0.01f };

        // Resizing of browser
        glm::vec2 _originalDimensions;
        float _originalScale;
        float _resizeAreaPercentage{ 0.1f };
        
        // Target & images
        ScreenSpaceSkyTarget* _skyTarget{ nullptr };
        std::thread _wwtMessages;       
        std::deque<int> _selectedImages;

        // Time variables
        // For capping the calls to change the zoom from scrolling
        constexpr static const std::chrono::milliseconds _timeUpdateInterval{ 10 };
        std::chrono::system_clock::time_point _lastUpdateTime;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

