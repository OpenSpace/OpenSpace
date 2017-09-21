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

#include <modules/base/rendering/renderableplane.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    enum BlendMode {
        BlendModeNormal = 0,
        BlendModeAdditive
    };

    static const openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };

    static const openspace::properties::Property::PropertyInfo BillboardInfo = {
        "Billboard",
        "Billboard mode",
        "This value specifies whether the plane is a billboard, which means that it is "
        "always facing the camera. If this is false, it can be oriented using other "
        "transformations."
    };

    static const openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the size of the plane in meters."
    };

    static const openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "This determines the blending mode that is applied to this plane."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderablePlane::Documentation() {
    using namespace documentation;
    return {
        "Renderable Plane",
        "base_renderable_plane",
        {
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SizeInfo.description
            },
            {
                BillboardInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                BillboardInfo.description
            },
            {
                BlendModeInfo.identifier,
                new StringInListVerifier({ "Normal", "Additive" }),
                Optional::Yes,
                BlendModeInfo.description, // + " The default value is 'Normal'.",
            },
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                TextureInfo.description,
            }
        }
    };
}


RenderablePlane::RenderablePlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _billboard(BillboardInfo, false)
    , _size(SizeInfo, 10.f, 0.f, 1e25f)
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _shader(nullptr)
    , _texture(nullptr)
    , _quad(0)
    , _vertexPositionBuffer(0)
    , _planeIsDirty(false)
    , _textureIsDirty(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlane"
    );

    _size = static_cast<float>(dictionary.value<double>(SizeInfo.identifier));

    if (dictionary.hasKey(BillboardInfo.identifier)) {
        _billboard = dictionary.value<bool>(BillboardInfo.identifier);
    }

    _blendMode.addOptions({
        { BlendModeNormal, "Normal" },
        { BlendModeAdditive, "Additive"}
    });
    _blendMode.onChange([&]() {
        switch (_blendMode) {
            case BlendModeNormal:
                setRenderBin(Renderable::RenderBin::Opaque);
                break;
            case BlendModeAdditive:
                setRenderBin(Renderable::RenderBin::Transparent);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    });

    if (dictionary.hasKey(BlendModeInfo.identifier)) {
        const std::string v = dictionary.value<std::string>(BlendModeInfo.identifier);
        if (v == "Normal") {
            _blendMode = BlendModeNormal;
        }
        else if (v == "Additive") {
            _blendMode = BlendModeAdditive;
        }
    }
    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);

    addProperty(_billboard);
    addProperty(_texturePath);
    _texturePath.onChange([this]() {loadTexture(); });
    _textureFile->setCallback(
        [this](const ghoul::filesystem::File&) { _textureIsDirty = true; }
    );

    addProperty(_size);
    _size.onChange([this](){ _planeIsDirty = true; });

    setBoundingSphere(_size);
}

bool RenderablePlane::isReady() const {
    return _shader && _texture;
}

void RenderablePlane::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    _shader = OsEng.renderEngine().buildRenderProgram("PlaneProgram",
        "${MODULE_BASE}/shaders/plane_vs.glsl",
        "${MODULE_BASE}/shaders/plane_fs.glsl"
    );

    loadTexture();
}

void RenderablePlane::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _textureFile = nullptr;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

void RenderablePlane::render(const RenderData& data, RendererTasks&) {
    _shader->activate();
    //if (_projectionListener){
    //    //get parent node-texture and set with correct dimensions  
    //    SceneGraphNode* textureNode = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName)->parent();
    //    if (textureNode != nullptr){
    //        RenderablePlanetProjection* t = static_cast<RenderablePlanetProjection*>(textureNode->renderable());
    //        _texture = std::unique_ptr<ghoul::opengl::Texture>(&(t->baseTexture()));
    //        unsigned int h = _texture->height();
    //        unsigned int w = _texture->width();
    //        float scale = static_cast<float>(h) / static_cast<float>(w);
    //        scaleTransform = glm::scale(glm::mat4(1.0), glm::vec3(1.f, scale, 1.f));
    //    }
    //}

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 rotationTransform = _billboard ?
        glm::inverse(glm::dmat4(data.camera.viewRotationMatrix())) :
        glm::dmat4(data.modelTransform.rotation);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(1.0);
    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));
    
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader->setUniform("additiveBlending", _blendMode == BlendModeAdditive);
    }

    bool additiveBlending = _blendMode == BlendModeAdditive && usingFramebufferRenderer;
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

void RenderablePlane::update(const UpdateData&) {
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
        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));

        if (texture) {
            LDEBUGC(
                "RenderablePlane",
                "Loaded texture from '" << absPath(_texturePath) << "'"
            );
            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);

            _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
            _textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
        }
    }
}

void RenderablePlane::createPlane() {
    const GLfloat size = _size;
    const GLfloat vertexData[] = {
        //      x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
        -size, size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, -size, 0.f, 0.f, 1.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        nullptr
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

} // namespace openspace
