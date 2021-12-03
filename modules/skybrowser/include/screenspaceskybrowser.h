#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <modules/skybrowser/include/wwtcommunicator.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace {

    class ScreenSpaceSkyBrowser : public ScreenSpaceRenderable, public WwtCommunicator
    {
    public:
        constexpr static const double FovThreshold = 0.001f;
        
        // Constructor and destructor
        ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
        ~ScreenSpaceSkyBrowser();

        // Inherited functions
        bool initializeGL() override;
        bool deinitializeGL() override;
        glm::mat4 scaleMatrix() override;
        void render() override;
        void update() override;

        // Animation
        bool isAnimated();
        void startFovAnimation(float fov);
        void incrementallyAnimateToFov(float deltaTime);

        // Getters
        float opacity() const;

        // Setters
        void setVerticalFovWithScroll(float scroll);
        void setScale(glm::vec2 scalingFactor);
        void setScale(float scalingFactor);
        void setOpacity(float opacity);
        // Set callback functions
        void setCallbackEquatorialAim(std::function<void(glm::dvec3, bool)> function);
        void setCallbackVerticalFov(std::function<void(float)> function);
        void setCallbackDimensions(std::function<void(const glm::vec2&)> function);
        void setCallbackBorderColor(std::function<void(const glm::ivec3&)> function);
        void setCallbackEnabled(std::function<void(bool)> function);

        // Interaction. Resize
        void saveResizeStartSize();       
        bool isOnResizeArea(glm::vec2 screenSpaceCoord);
        void resize(const glm::vec2& start, const glm::vec2& mouseDrag);

        glm::dvec2 fineTuneVector(glm::dvec2 drag);
        void setIdInBrowser();

        void updateTextureResolution();

    private:
        properties::DoubleProperty _animationSpeed;
        properties::FloatProperty _textureQuality;

        void bindTexture() override;

        // Flags
        bool _isSyncedWithWwt{ false };
        bool _isFovAnimated{ false };

        // Animation of fieldOfView
        float _endVfov{ 0.f };

        // Resizing
        float _originalScale;
        float _resizeAreaPercentage{ 0.1f };
        glm::vec2 _originalDimensions;
        glm::vec2 _originalScreenSpaceSize;
        glm::ivec2 _resizeDirection;

        // Time variables
        // For capping the calls onchange properties from scrolling
        constexpr static const std::chrono::milliseconds _timeUpdateInterval{ 10 };
        std::chrono::system_clock::time_point _lastUpdateTime;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
