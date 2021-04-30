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

        glm::dvec3 getTargetDirection();
        glm::dvec2 getScreenSpacePosition();
        void setConnectedBrowser();
        void setBorderColor(glm::ivec3 color);
        glm::ivec3 getColor();
       
        void translate(glm::dvec2 translation, glm::dvec2 position);

        glm::dvec2 getScreenSpaceDimensions();
        glm::dvec2 getUpperRightCornerScreenSpace();
        glm::dvec2 getLowerLeftCornerScreenSpace();
        bool coordIsInsideCornersScreenSpace(glm::dvec2 coord);

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

