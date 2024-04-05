/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

// @TODO: Clean up this class and make it not copy much code from RenderablePlane ---abock

#include <modules/debugging/rendering/renderabledebugplane.h>

#include <modules/spacecraftinstruments/rendering/renderableplanetprojection.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>

namespace {
    enum Origin {
        LowerLeft = 0,
        LowerRight,
        UpperLeft,
        UpperRight,
        Center
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The OpenGL name of the texture that is displayed on this plane",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardInfo = {
        "Billboard",
        "Billboard mode",
        "This value specifies whether the plane is a billboard, which means that it is "
        "always facing the camera. If this is false, it can be oriented using other "
        "transformations",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the size of the plane in meters",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OriginInfo = {
        "Origin",
        "Texture Coordinate Origin",
        "The origin of the texture coorinate system",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableDebugPlane)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::optional<int> texture;

        // [[codegen::verbatim(BillboardInfo.description)]]
        std::optional<bool> billboard;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        enum class [[codegen::map(Origin)]] Origin {
            LowerLeft,
            LowerRight,
            UpperLeft,
            UpperRight,
            Center
        };
        // [[codegen::verbatim(OriginInfo.description)]]
        std::optional<Origin> origin;
    };
#include "renderabledebugplane_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableDebugPlane::Documentation() {
    return codegen::doc<Parameters>("debugging_renderable_debugplane");
}

RenderableDebugPlane::RenderableDebugPlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texture(TextureInfo, -1, -1, 512)
    , _billboard(BillboardInfo, false)
    , _size(SizeInfo, 10.f, 0.f, 1e25f)
    , _origin(OriginInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texture = p.texture.value_or(_texture);
    addProperty(_texture);

    _size.setExponent(15.f);
    _size.onChange([this](){ _planeIsDirty = true; });
    _size = p.size.value_or(_size);
    setBoundingSphere(_size);
    addProperty(_size);

    _billboard = p.billboard.value_or(_billboard);
    addProperty(_billboard);

    _origin.addOptions({
        { LowerLeft, "LowerLeft" },
        { LowerRight, "LowerRight" },
        { UpperLeft, "UpperLeft" },
        { UpperRight, "UpperRight" },
        { Center, "Center" }
    });
    _origin.setValue(Center);

    if (p.origin.has_value()) {
        _origin = codegen::map<Origin>(*p.origin);
    }
    else {
        _origin = Center;
    }
}

bool RenderableDebugPlane::isReady() const {
    bool ready = true;
    if (!_shader) {
        ready &= false;
    }
    return ready;
}

void RenderableDebugPlane::initializeGL() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    if (!_shader) {
        _shader = global::renderEngine->buildRenderProgram("PlaneProgram",
            absPath("${MODULE_BASE}/shaders/plane_vs.glsl"),
            absPath("${MODULE_BASE}/shaders/plane_fs.glsl")
        );
    }
}

void RenderableDebugPlane::deinitializeGL() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

void RenderableDebugPlane::render(const RenderData& data, RendererTasks&) {
    glm::mat4 transform = glm::mat4(1.f);
    if (_billboard) {
        transform = glm::inverse(glm::mat4(data.camera.viewRotationMatrix()));
    }

    // Activate shader
    _shader->activate();

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    _shader->setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    _shader->setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    _shader->setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    _shader->setUniform("scaling", glm::vec2(1.f, 0.f));

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, _texture);
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _shader->deactivate();
}

void RenderableDebugPlane::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    if (_planeIsDirty) {
        createPlane();
    }
}

void RenderableDebugPlane::createPlane() {
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat size = _size;

    const std::array<GLfloat, 36> vertexData = {
        //  x      y    z    w    s    t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
         size,  size, 0.f, 0.f, 1.f, 1.f,
        -size,  size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
         size, -size, 0.f, 0.f, 1.f, 0.f,
         size,  size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
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
