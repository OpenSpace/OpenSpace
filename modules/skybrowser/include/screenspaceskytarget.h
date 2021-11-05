#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/numericalproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class ScreenSpaceSkyBrowser;

    class ScreenSpaceSkyTarget : public ScreenSpaceRenderable {
    
    public:
        // Constructor & destructor
        ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyTarget();

        // ScreenSpaceRenderable inherited functions
        bool initializeGL() override;
        bool deinitializeGL() override;
        bool isReady() const override;
        void render() override;
        glm::mat4 scaleMatrix() override;
        void bindTexture() override; // Empty function but has to be defined
        void createShaders();

        // Sky browser functionality
        bool findSkyBrowser();
        void matchAppearanceToSkyBrowser();
        
        // Getters
        ScreenSpaceSkyBrowser* getSkyBrowser();
        glm::ivec3 borderColor() const;
        float opacity() const;

        // Setters
        void setScale(float verticalFov);
        void setDimensions(glm::vec2 dimensions);
        void setColor(glm::ivec3 color);
        void setOpacity(float opacity);

        // Target directions
        glm::dvec3 directionGalactic() const;
        glm::dvec3 directionEquatorial() const;

        // Locking functionality
        void lock();
        void unlock();
        bool isLocked();

        // Animation
        bool isAnimated();
        void startAnimation(glm::dvec3 end, bool lockAfter);
        void animateToCoordinate(float deltaTime);
        // Display
        void highlight(glm::ivec3 addition);
        void removeHighlight(glm::ivec3 removal);

    private:
        // Properties
        properties::StringProperty _skyBrowserId;
        properties::FloatProperty _showCrosshairThreshold;
        properties::FloatProperty _showRectangleThreshold;
        properties::DoubleProperty _stopAnimationThreshold;
        properties::DoubleProperty _animationSpeed;

        // Flags
        bool _isLocked{ false };
        bool _isAnimated{ false };
        bool _lockAfterAnimation{ false };

        // Shader
        UniformCache(modelTransform, viewProj, showCrosshair, showRectangle, lineWidth, dimensions, lineColor) _uniformCache;
        GLuint _vertexArray = 0;
        GLuint _vertexBuffer = 0;
        
        // Sky browser
        ScreenSpaceSkyBrowser* _skyBrowser;
        glm::ivec3 _color;
        
        // Lock target to a coordinate on the sky
        glm::dvec3 _lockedCoordinates;              // Spherical celestial coordinates
        std::thread _lockTarget;

        glm::dvec3 _animationEnd;        // Cartesian celestial coordinates
        glm::dvec3 _animationStart;      // Cartesian celestial coordinates
        
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

