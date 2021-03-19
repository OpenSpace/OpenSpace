#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/numericalproperty.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <ghoul/opengl/texture.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class ScreenSpaceSkyTarget : public ScreenSpaceRenderable {
    
    public:
        ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyTarget() = default;

        // from SSR
        bool initializeGL() override;
        bool isReady() const override;

        void render() override;
        void update() override;

        void createShaders();

        void setScreenSpaceTargetDimension(glm::vec2 currentBrowserDimension);

        glm::vec2 getScreenSpacePosition();
        glm::vec2 getAnglePosition();
        void setScale(float scale);
       
        void translate(glm::vec2 translation, glm::vec2 position);
       
        glm::vec2 getScreenSpaceDimensions();
        glm::vec2 getUpperRightCornerScreenSpace();
        glm::vec2 getLowerLeftCornerScreenSpace();
        bool coordIsInsideCornersScreenSpace(glm::vec2 coord);

        glm::mat4 scaleMatrix() override;
        
        void bindTexture() override;
    private:

        properties::Vec2Property _targetDimensions;

        std::unique_ptr<ghoul::opengl::Texture> _texture;
        //glm::vec2 _browserDimension = glm::vec2(0);
        UniformCache(modelTransform, viewProj, texture, borderWidth, targetRatio) _uniformCache;
        GLuint _vertexArray = 0;
        GLuint _vertexBuffer = 0;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

