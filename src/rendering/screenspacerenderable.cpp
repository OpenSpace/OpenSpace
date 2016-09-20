/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/camera.h>
#include <openspace/util/factorymanager.h>

#include <openspace/documentation/verifier.h>

 #ifdef WIN32
 #define _USE_MATH_DEFINES
 #include <math.h>
 #endif

namespace {
    const std::string _loggerCat = "ScreenSpaceRenderable";

    const std::string KeyType = "Type";
    const std::string KeyFlatScreen = "FlatScreen";
    const std::string KeyPosition = "Position";
    const std::string KeyScale = "Scale";
    const std::string KeyDepth = "Depth";
    const std::string KeyAlpha = "Alpha";

    const float PlaneDepth = -2.f;
}

namespace openspace {

Documentation ScreenSpaceRenderable::Documentation() {
    using namespace openspace::documentation;

    return {
        "Screenspace Renderable",
        "core_screenspacerenderable",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Screenspace renderable"),
                "The type of the Screenspace renderable that is to be created. The "
                "available types of Screenspace renderable depend on the configuration of"
                "the application and can be written to disk on application startup into "
                "the FactoryDocumentation.",
                Optional::No
            }
        }
    };
}

ScreenSpaceRenderable* ScreenSpaceRenderable::createFromDictionary(
    const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceRenderable"
    );

    std::string renderableType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ScreenSpaceRenderable* result = factory->create(renderableType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a ScreenSpaceRenderable object of type '" <<
               renderableType << "'"
        );
        return nullptr;
    }

    return result;
}


ScreenSpaceRenderable::ScreenSpaceRenderable(const ghoul::Dictionary& dictionary)
    : _enabled("enabled", "Is Enabled", true)
    , _useFlatScreen("flatScreen", "Flat Screen", true)
    , _euclideanPosition(
        "euclideanPosition",
        "Euclidean coordinates",
        glm::vec2(0.f),
        glm::vec2(-4.f),
        glm::vec2(4.f)
    )
    , _sphericalPosition(
        "sphericalPosition",
        "Spherical coordinates",
        glm::vec2(0.f, M_PI_2),
        glm::vec2(-M_PI),
        glm::vec2(M_PI)
    )
    , _depth("depth", "Depth", 0.f, 0.f, 1.f)
    , _scale("scale", "Scale", 0.25f, 0.f, 2.f)
    , _alpha("alpha", "Alpha", 1.f, 0.f, 1.f)
    , _delete("delete", "Delete")
    , _quad(0)
    , _vertexPositionBuffer(0)
    , _texture(nullptr)
    , _shader(nullptr)
    , _radius(PlaneDepth)
{
    addProperty(_enabled);
    addProperty(_useFlatScreen);
    addProperty(_depth);
    addProperty(_scale);
    addProperty(_alpha);
    addProperty(_delete);


    dictionary.getValue(KeyFlatScreen, _useFlatScreen);
    useEuclideanCoordinates(_useFlatScreen);
    
    if (_useFlatScreen)
        dictionary.getValue(KeyPosition, _euclideanPosition);
    else
        dictionary.getValue(KeyPosition, _sphericalPosition);


    dictionary.getValue(KeyScale, _scale);
    dictionary.getValue(KeyDepth, _depth);
    dictionary.getValue(KeyAlpha, _alpha);

    // Setting spherical/euclidean onchange handler
    _useFlatScreen.onChange([this](){   
        if (_useFlatScreen) {
            addProperty(_euclideanPosition);
            removeProperty(_sphericalPosition);
        } else {
            removeProperty(_euclideanPosition);
            addProperty(_sphericalPosition);
        }
        useEuclideanCoordinates(_useFlatScreen);
    });

    _delete.onChange([this](){
        std::string script = 
            "openspace.unregisterScreenSpaceRenderable('" + name() + "');";
        OsEng.scriptEngine().queueScript(script);
    });
}

ScreenSpaceRenderable::~ScreenSpaceRenderable() {}

bool ScreenSpaceRenderable::isEnabled() const {
    return _enabled;
}

glm::vec3 ScreenSpaceRenderable::euclideanPosition() const {
    return glm::vec3(_euclideanPosition.value(), _depth.value());
}

glm::vec3 ScreenSpaceRenderable::sphericalPosition() const {
    return glm::vec3(_sphericalPosition.value(), _depth.value());
}

float ScreenSpaceRenderable::depth() const {
    return _depth;
}

void ScreenSpaceRenderable::createPlane() {
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat vertex_data[] = {
        //      x      y     z     w     s     t
        -1, -1, 0.0f, 1, 0, 0,
         1,  1, 0.0f, 1, 1, 1,
        -1,  1, 0.0f, 1, 0, 1,
        -1, -1, 0.0f, 1, 0, 0,
         1, -1, 0.0f, 1, 1, 0,
         1,  1, 0.0f, 1, 1, 1,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(0)
    );
    
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );
}

void ScreenSpaceRenderable::useEuclideanCoordinates(bool b) {
    _useEuclideanCoordinates = b;
    if (_useEuclideanCoordinates) {
        _euclideanPosition = toEuclidean(_sphericalPosition.value(), _radius);
    } else {
        _sphericalPosition = toSpherical(_euclideanPosition.value());
    }
}

glm::vec2 ScreenSpaceRenderable::toEuclidean(const glm::vec2& spherical, float r) {
    float x = r * sin(spherical[0]) * sin(spherical[1]);
    float y = r * cos(spherical[1]);
    
    return glm::vec2(x, y);
}

glm::vec2 ScreenSpaceRenderable::toSpherical(const glm::vec2& euclidean) {
    _radius = -sqrt(pow(euclidean[0],2)+pow(euclidean[1],2)+pow(PlaneDepth,2));
    float theta = atan2(-PlaneDepth,euclidean[0])-M_PI/2.0;
    float phi = acos(euclidean[1]/_radius);

    return glm::vec2(theta, phi);
}

void ScreenSpaceRenderable::createShaders() {
    if (!_shader) {
        ghoul::Dictionary dict = ghoul::Dictionary();

        auto res = OsEng.windowWrapper().currentWindowResolution();
        ghoul::Dictionary rendererData = {
            { "fragmentRendererPath", "${SHADERS}/framebuffer/renderframebuffer.frag" },
            { "windowWidth" , res.x },
            { "windowHeight" , res.y }
        };

        dict.setValue("rendererData", rendererData);
        dict.setValue("fragmentPath", "${MODULE_BASE}/shaders/screenspace_fs.glsl");
        _shader = ghoul::opengl::ProgramObject::Build(
            "ScreenSpaceProgram",
            "${MODULE_BASE}/shaders/screenspace_vs.glsl",
            "${SHADERS}/render.frag",
            dict
        );
    }
}

glm::mat4 ScreenSpaceRenderable::scaleMatrix() {
    glm::vec2 resolution = OsEng.windowWrapper().currentWindowResolution();

    //to scale the plane
    float textureRatio =
        static_cast<float>(_texture->height()) / static_cast<float>(_texture->width());
        
    float scalingRatioX = _originalViewportSize.x / resolution.x;
    float scalingRatioY = _originalViewportSize.y / resolution.y;
    return glm::scale(
        glm::mat4(1.f),
        glm::vec3(
            _scale * scalingRatioX,
            _scale * scalingRatioY * textureRatio,
            1.f
        )
    ); 
}

glm::mat4 ScreenSpaceRenderable::rotationMatrix() {
    // Get the scene transform
    glm::mat4 rotation = OsEng.windowWrapper().modelMatrix();
    if (!_useEuclideanCoordinates) {
        glm::vec2 position = _sphericalPosition.value();

        rotation = glm::rotate(rotation, position.x, glm::vec3(0.f, 1.f, 0.f));
        rotation = glm::rotate(
            rotation,
            static_cast<float>(position.y - M_PI_2),
            glm::vec3(1.f, 0.f, 0.f)
        );
    }

    return rotation;
}

glm::mat4 ScreenSpaceRenderable::translationMatrix() {
    glm::mat4 translation(1.0);
    if (!_useEuclideanCoordinates) {
        translation = glm::translate(translation, glm::vec3(0.0f, 0.0f, PlaneDepth));
    } else {
        translation = glm::translate(
            glm::mat4(1.f),
            glm::vec3(_euclideanPosition.value(), PlaneDepth)
        );
    }

    return translation;
}

void ScreenSpaceRenderable::draw(glm::mat4 modelTransform) {
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    _shader->activate();
    _shader->setUniform("OcclusionDepth", 1.f - _depth);
    _shader->setUniform("Alpha", _alpha);
    _shader->setUniform("ModelTransform", modelTransform);
    _shader->setUniform(
        "ViewProjectionMatrix",
        OsEng.renderEngine().camera()->viewProjectionMatrix()
    );
    
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    
    glEnable(GL_CULL_FACE);

    _shader->deactivate();
}

} // namespace openspace
