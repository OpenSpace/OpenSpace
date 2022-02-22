#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class ScreenSpaceSkyBrowser;

    class ScreenSpaceSkyTarget : public ScreenSpaceRenderable {
    
    public:
        constexpr static const float DeltaTimeThreshold = 0.03f; 

        // Constructor & destructor
        ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyTarget();

        // ScreenSpaceRenderable inherited functions
        bool initializeGL() override;
        bool deinitializeGL() override;
        bool isReady() const override;
        void render() override;
        void update() override;
        glm::mat4 scaleMatrix() override;
        void bindTexture() override; // Empty function but has to be defined
        void createShaders();

        glm::ivec3 borderColor() const;
        float opacity() const;
        glm::dvec2 lockedCoordinates() const;

        // Setters
        void setScaleFromVfov(float verticalFov);
        void setFovFromScale();
        void setDimensions(glm::vec2 dimensions);
        void setColor(glm::ivec3 color);
        void setOpacity(float opacity);
        void setLock(bool isLocked);
        void setEquatorialAim(const glm::dvec2& aim);

        // Target directions
        glm::dvec3 directionGalactic() const;
        glm::dvec2 equatorialAim() const;

        // Locking functionality
        bool isLocked() const;

        // Animation
        bool isAnimated();
        void startAnimation(glm::dvec3 end, bool shouldLockAfter = true);
        void incrementallyAnimateToCoordinate(float deltaTime);
        // Display
        void highlight(glm::ivec3 addition);
        void removeHighlight(glm::ivec3 removal);

    private:
        // Properties
        properties::FloatProperty _showCrosshairThreshold;
        properties::FloatProperty _showRectangleThreshold;
        properties::FloatProperty _lineWidth;
        properties::DoubleProperty _stopAnimationThreshold;
        properties::DoubleProperty _animationSpeed;

        // Flags
        bool _isLocked{ false };
        bool _isAnimated{ false };
        bool _shouldLockAfterAnimation{ false };

        // Shader
        UniformCache(
            modelTransform, 
            viewProj, 
            showCrosshair, 
            showRectangle, 
            lineWidth,
            dimensions, 
            lineColor) _uniformCache;
        GLuint _vertexArray = 0;
        GLuint _vertexBuffer = 0;
        
        // Sky browser
        float _verticalFov{ 70.0f };
        glm::dvec2 _equatorialAim{ 0.0 };
        glm::ivec3 _borderColor{ 255 };

        // Lock target to a coordinate on the sky
        glm::dvec3 _lockedCoordinates;   // Cartesian equatorial coordinates

        // Animation
        glm::dvec3 _animationEnd;        // Cartesian equatorial coordinates
        glm::dvec3 _animationStart;      // Cartesian equatorial coordinates
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

