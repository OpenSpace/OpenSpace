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
        ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyTarget();

        bool initializeGL() override;
        bool isReady() const override;
        void render() override;
        glm::mat4 scaleMatrix() override;
        void bindTexture() override; // Empty function but has to be defined

        void createShaders();
        void initializeWithBrowser();
        ScreenSpaceSkyBrowser* getSkyBrowser();
       
        void setVerticalFOV(float VFOV);
        void setDimensions(glm::vec2 dimensions);
        void setColor(glm::ivec3 color);
        glm::ivec3 getColor();
        properties::FloatProperty& getOpacity();

        // Target directions
        glm::dvec3 getTargetDirectionGalactic();
        glm::dvec2 getTargetDirectionCelestial();

        // Locking functionality
        void unlock();
        void lock();
        bool isLocked();
        
        // Animations
        void startAnimation(glm::dvec2 coordsEnd, float FOVEnd, bool lockAfterwards = true);
        void animateToCoord(double deltaTime);
        bool animateToFOV(float endFOV, float deltaTime);

        properties::StringProperty _skyBrowserID;
        properties::FloatProperty _showCrosshairThreshold;
        properties::FloatProperty _showRectangleThreshold;

    private:
        // Shader
        UniformCache(modelTransform, viewProj, showCrosshair, showRectangle, lineWidth, dimensions, lineColor) _uniformCache;
        GLuint _vertexArray = 0;
        GLuint _vertexBuffer = 0;
        glm::ivec3 _color;
        float _verticalFOV = 100.f;
        ScreenSpaceSkyBrowser* _skyBrowser;
        
        // Locking target to a coordinate on the sky
        bool _isLocked;
        glm::dvec2 _lockedCoords;              // Spherical celestial coords
        std::thread _lockTargetThread;

        // Animating the target
        bool _isAnimated = false;
        glm::dvec3 _coordsEndAnimation;        // Cartesian celestial coords
        glm::dvec3 _coordsStartAnimation;      // Cartesian celestial coords
        double _animationTime = 1.0;
        float _FOVEndAnimation;
        bool _lockAfterAnimation;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

