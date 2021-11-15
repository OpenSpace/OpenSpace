#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__

#include <modules/skybrowser/include/wwtcommunicator.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec3property.h>
#include <deque>

namespace openspace {
    class ScreenSpaceSkyTarget;

    class ScreenSpaceSkyBrowser2 : public ScreenSpaceRenderable, public WwtCommunicator
    {
    public:
        // Constructor and destructor
        ScreenSpaceSkyBrowser2(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyBrowser2();

        // Inherited functions
        bool initializeGL() override;
        bool deinitializeGL() override;
        glm::mat4 scaleMatrix() override;
        void render() override;
        void update() override;

        // Target - browser connection
        bool connectToSkyTarget();
        bool isAnimated();
        void startFovAnimation(float fov);
        void incrementallyAnimateToFov(float deltaTime);   
        glm::dvec2 fineTuneVector(glm::dvec2 drag);

        // Getters returning references
        ScreenSpaceSkyTarget* getSkyTarget();
        properties::FloatProperty& getOpacity();

        // Setters
        void setVerticalFovWithScroll(float scroll);
        void setScale(glm::vec2 scalingFactor);
        void setScale(float scalingFactor);

        // Communication with WorldWide Telescope
        void startSyncingWithWwt();
        

        // Mouse interaction with the browser. Returns 1 or -1 at the coordinate in
        // image if the mouse is on a side of the browser
        //            __1__
        //   y|   -1 |_____|1
        //    |__x     -1
        glm::ivec2 isOnResizeArea(glm::vec2 screenSpaceCoord);

    private:
        void syncWwtView();
        void bindTexture() override;

        // Resize functions
        void saveResizeStartSize();
        void updateBrowserSize();

        // Properties
        properties::StringProperty _skyTargetId;

        // Flags
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

        // Time variables
        // For capping the calls to change the zoom from scrolling
        constexpr static const std::chrono::milliseconds _timeUpdateInterval{ 10 };
        std::chrono::system_clock::time_point _lastUpdateTime;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
