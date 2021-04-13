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
        virtual ~ScreenSpaceSkyTarget() = default;

        bool initializeGL() override;
        bool isReady() const override;
        void render() override;
        void createShaders();

        void setBrowser(ScreenSpaceSkyBrowser* browser);
        ScreenSpaceSkyBrowser* getSkyBrowser();

        void setDimensions(glm::vec2 currentBrowserDimensions);
        void updateFOV(float browserFOV);

        glm::dvec2 convertGalacticToCelestial(glm::dvec3 rGal) const;
        glm::vec2 getCelestialCoords();
        glm::vec2 getScreenSpacePosition();
        glm::vec2 getAnglePosition();
        void setConnectedBrowser();
        void setBorderColor(glm::ivec3 color);
        glm::ivec3 getColor();

        void setPosition(glm::vec3 pos);
       
        void translate(glm::vec2 translation, glm::vec2 position);
        // Only works for galactic coords outside of the solar system
        void lookAtGalacticCoord(glm::dvec3 galacticCoord);

        glm::vec2 getScreenSpaceDimensions();
        glm::vec2 getUpperRightCornerScreenSpace();
        glm::vec2 getLowerLeftCornerScreenSpace();
        bool coordIsInsideCornersScreenSpace(glm::vec2 coord);

        glm::mat4 scaleMatrix() override;
        
        void bindTexture() override;
        properties::StringProperty _skyBrowserID;
    private:
    
        properties::Vec2Property _targetDimensions;
        properties::FloatProperty _showCrosshairThreshold;
        std::unique_ptr<ghoul::opengl::Texture> _texture;

        UniformCache(modelTransform, viewProj, texture, showCrosshair, borderWidth, targetDimensions, borderColor) _uniformCache;
        GLuint _vertexArray = 0;
        GLuint _vertexBuffer = 0;
        float _fieldOfView = 100.f;
        ScreenSpaceSkyBrowser* _skyBrowser;
        glm::vec3 _borderColor;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

