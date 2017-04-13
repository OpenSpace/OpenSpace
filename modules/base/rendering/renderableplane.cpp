/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/engine/configurationmanager.h>
#include <modules/base/rendering/renderableplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderengine.h>
#include <modules/newhorizons/rendering/renderableplanetprojection.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    static const std::string _loggerCat = "RenderablePlane";

    const char* keyFieldlines = "Fieldlines";
    const char* keyFilename = "File";
    const char* keyHints = "Hints";
    const char* keyShaders = "Shaders";
    const char* keyVertexShader = "VertexShader";
    const char* keyFragmentShader = "FragmentShader";
}

namespace openspace {

RenderablePlane::RenderablePlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath("texture", "Texture")
    , _billboard("billboard", "Billboard", false)
    , _projectionListener("projectionListener", "DisplayProjections", false)
    , _size("size", "Size", 10, 0, std::pow(10, 25))
    , _origin(Origin::Center)
    , _shader(nullptr)
    , _textureIsDirty(false)
    , _texture(nullptr)
    , _blendMode(BlendMode::Normal)
    , _quad(0)
    , _vertexPositionBuffer(0)
{
    dictionary.getValue("Size", _size);

    if (dictionary.hasKey("Name")) {
        dictionary.getValue("Name", _nodeName);
    }

    std::string origin;
    if (dictionary.getValue("Origin", origin)) {
        if (origin == "LowerLeft") {
            _origin = Origin::LowerLeft;
        }
        else if (origin == "LowerRight") {
            _origin = Origin::LowerRight;
        }
        else if (origin == "UpperLeft") {
            _origin = Origin::UpperLeft;
        }
        else if (origin == "UpperRight") {
            _origin = Origin::UpperRight;
        }
        else if (origin == "Center") {
            _origin = Origin::Center;
        }
    }

    // Attempt to get the billboard value
    bool billboard = false;
    if (dictionary.getValue("Billboard", billboard)) {
        _billboard = billboard;
    }
    if (dictionary.hasKey("ProjectionListener")){
        bool projectionListener = false;
        if (dictionary.getValue("ProjectionListener", projectionListener)) {
            _projectionListener = projectionListener;
        }
    }

    std::string blendMode;
    if (dictionary.getValue("BlendMode", blendMode)) {
        if (blendMode == "Additive") {
            _blendMode = BlendMode::Additive;
            setRenderBin(Renderable::RenderBin::Transparent);
        }
    }

    std::string texturePath = "";
    bool success = dictionary.getValue("Texture", texturePath);
    if (success) {
        _texturePath = absPath(texturePath);
        _textureFile = new ghoul::filesystem::File(_texturePath);
    }

    addProperty(_billboard);
    addProperty(_texturePath);
    _texturePath.onChange(std::bind(&RenderablePlane::loadTexture, this));
    _textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });

    addProperty(_size);
    //_size.onChange(std::bind(&RenderablePlane::createPlane, this));
    _size.onChange([this](){ _planeIsDirty = true; });

    setBoundingSphere(_size);
}

RenderablePlane::~RenderablePlane() {
    delete _textureFile;
    _textureFile = nullptr;
}

bool RenderablePlane::isReady() const {
    bool ready = true;
    if (!_shader)
        ready &= false;
    if(!_texture)
        ready &= false;
    return ready;
}

bool RenderablePlane::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    if (_shader == nullptr) {
        // Plane Program

        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("PlaneProgram",
            "${MODULE_BASE}/shaders/plane_vs.glsl",
            "${MODULE_BASE}/shaders/plane_fs.glsl"
            );
        if (!_shader)
            return false;
    }

    loadTexture();

    return isReady();
}

bool RenderablePlane::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (!_projectionListener){
        // its parents job to kill texture
        // iff projectionlistener 
        _texture = nullptr;
    }

    delete _textureFile;
    _textureFile = nullptr;


    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void RenderablePlane::render(const RenderData& data) {
    glm::mat4 scaleTransform = glm::mat4(1.0);
    
    // Activate shader
    _shader->activate();
    if (_projectionListener){
        //get parent node-texture and set with correct dimensions  
        SceneGraphNode* textureNode = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName)->parent();
        if (textureNode != nullptr){
            RenderablePlanetProjection* t = static_cast<RenderablePlanetProjection*>(textureNode->renderable());
            _texture = std::unique_ptr<ghoul::opengl::Texture>(&(t->baseTexture()));
            unsigned int h = _texture->height();
            unsigned int w = _texture->width();
            float scale = static_cast<float>(h) / static_cast<float>(w);
            scaleTransform = glm::scale(glm::mat4(1.0), glm::vec3(1.f, scale, 1.f));
        }
    }

    // Model transform and view transform needs to be in double precision
    glm::dmat4 rotationTransform;
    if (_billboard)
        rotationTransform = glm::inverse(glm::dmat4(data.camera.viewRotationMatrix()));
    else
        rotationTransform = glm::dmat4(data.modelTransform.rotation);

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(scaleTransform);
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));
    
    //_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    //_shader->setUniform("ModelTransform", transform);
    //setPscUniforms(*_shader.get(), data.camera, data.position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader->setUniform("additiveBlending", _blendMode == BlendMode::Additive);
    }

    bool additiveBlending = _blendMode == BlendMode::Additive && usingFramebufferRenderer;
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->deactivate();
}

void RenderablePlane::update(const UpdateData& data) {
    if (_shader->isDirty())
        _shader->rebuildFromFile();

    if (_planeIsDirty)
        createPlane();

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlane::loadTexture() {
    if (_texturePath.value() != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
        if (texture) {
            LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);

            delete _textureFile;
            _textureFile = new ghoul::filesystem::File(_texturePath);
            _textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
        }
    }
}

void RenderablePlane::createPlane() {
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat size = _size;
    const GLfloat vertex_data[] = {
        //      x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
        -size, size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, -size, 0.f, 0.f, 1.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

} // namespace openspace
