#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <modules/skybrowser/include/wwtcommunicator.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>

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
        glm::vec2 size() const;

        // Setters
        void setVerticalFovWithScroll(float scroll);
        void setOpacity(float opacity);
        void setScreenSpaceSize(const glm::vec2& newSize);
        void updateScreenSpaceSize();

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
        bool _textureDimensionsIsDirty{ false };
        bool _sizeIsDirty{ false };

        // Animation of fieldOfView
        float _endVfov{ 0.f };
        glm::vec2 _size{ 0.5f };
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER2___H__
