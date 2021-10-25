#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
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
        glm::dvec3 targetDirectionGalactic() const;
        glm::dvec3 targetDirectionEquatorial() const;

        // Locking functionality
        void lock();
        void unlock();
        bool isLocked();
        
        // Animations
        void startAnimation(glm::dvec3 coordsEnd, float FOVEnd, bool lockAfter = true);
        void animateToCoordinate(double deltaTime);
        bool animateToFov(float endFOV, float deltaTime);

    private:
        // Properties
        properties::StringProperty _skyBrowserId;
        properties::FloatProperty _showCrosshairThreshold;
        properties::FloatProperty _showRectangleThreshold;

        // Flags
        bool _isAnimated{ false };
        bool _lockAfterAnimation{ false };
        bool _isLocked{ false };

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

        // Animation of target
        glm::dvec3 _animationEnd;        // Cartesian celestial coordinates
        glm::dvec3 _animationStart;      // Cartesian celestial coordinates
        double _animationTime = 1.0;
        float _vfovEndAnimation;
        
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

