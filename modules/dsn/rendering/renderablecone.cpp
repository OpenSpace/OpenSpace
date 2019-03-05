/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/dsn/rendering/renderablecone.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const char* ProgramName = "ConeProgram";
    constexpr const char* _loggerCat = "RenderableCone";


    constexpr const std::array <const char*, openspace::RenderableCone::uniformCacheSize> UniformNames = {
        "modelView", "projectionTransform"};

    constexpr openspace::properties::Property::PropertyInfo ApexPositionInfo = {
        "ApexPosition",
        "Apex Position",
        "This value specifies the position of the cone apex. If this value"
        "is a string, it is interpreted as the identifier of another "
        "scenegraph node. If this value is a 3-vector, it is interpreted "
        "as a 3D world position."
    };

    constexpr openspace::properties::Property::PropertyInfo BaseCenterDirectionInfo = {
        "BaseCenterDirection",
        "Base Center Direction",
        "This value specifies the direction from the apex to the base center of "
        "the cone. If this value is a string, it is interpreted as the identifier " 
        "of another scenegraph node. If this value is a 3-vector, it is interpreted "
        "as a 3D direction vector."
    };

    constexpr openspace::properties::Property::PropertyInfo ReverseDirectionInfo = {
        "ReverseDirection",
        "Reverse Direction",
        "Reverses the BaseCenterDirection"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "Color of the cone"
    };

    constexpr openspace::properties::Property::PropertyInfo HeightInfo = {
        "Height",
        "Height",
        "Height of the cone"
    };
    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "Radius of the cone base"
    };
    constexpr openspace::properties::Property::PropertyInfo ResolutionInfo = {
        "Resolution",
        "Resolution",
        "Resolution of the cone, i.e number of vertices around the base"
    };
    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the transparency of this object."
    };
    constexpr openspace::properties::Property::PropertyInfo WireframeInfo = {
        "Wireframe",
        "Wireframe",
        "This value determines if the FOV is renderd in wireframe or not."
    };
} // namespace

namespace openspace {


documentation::Documentation RenderableCone::Documentation() {
    using namespace documentation;
    return {
        "Renderable Cone",
        "dsn_renderable_renderablecone",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableCone"),
                Optional::Yes
            },
            {
                ApexPositionInfo.identifier,
                new OrVerifier({ new StringVerifier, new DoubleVector3Verifier, }),
                Optional::No,
                ApexPositionInfo.description
            },
            {
                BaseCenterDirectionInfo.identifier,
                new OrVerifier({ new StringVerifier, new DoubleVector3Verifier, }),
                Optional::No,
                BaseCenterDirectionInfo.description
            },
            {
                ReverseDirectionInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                ReverseDirectionInfo.description
            },
            {
                ColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                ColorInfo.description
            },
            {
                HeightInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                HeightInfo.description
            },
            {
                RadiusInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                RadiusInfo.description
            },
            {
                ResolutionInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ResolutionInfo.description
            },
            {
                OpacityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                OpacityInfo.description
            },
            {
                WireframeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                WireframeInfo.description
            }
        }
    };
}

RenderableCone::RenderableCone(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _height(HeightInfo, 0.25, 0.0, 1.0)
    , _radius(RadiusInfo, 0.8, 0.0, 1.0)
    , _resolution(ResolutionInfo, 50, 4, 100)
    , _wireframe(WireframeInfo, false)
    , _color(
        ColorInfo,
        _defaultColor,
        glm::vec3(0.0), 
        glm::vec3(1.0)
    )
{

    if (dictionary.hasKey(ApexPositionInfo.identifier)) {

        if (dictionary.hasKeyAndValue<std::string>(ApexPositionInfo.identifier)) {
            _apexNodeId = dictionary.value<std::string>(ApexPositionInfo.identifier);
        }
        else {
            // We know it has to be a vector now
            _apexPosition = dictionary.value<glm::dvec3>(ApexPositionInfo.identifier);
            _apexIsNodeAttached = false;
        }
    }

    if (dictionary.hasKey(BaseCenterDirectionInfo.identifier)) {

        if (dictionary.hasKeyAndValue<std::string>(BaseCenterDirectionInfo.identifier)) {
            _baseDirNodeId = dictionary.value<std::string>(BaseCenterDirectionInfo.identifier);
        }
        else {
            // We know it has to be a vector now
            _baseCenterDirection = dictionary.value<glm::dvec3>(ApexPositionInfo.identifier);
            _baseCenterIsNodeAttached = false;
        }

        if (dictionary.hasKeyAndValue<bool>(ReverseDirectionInfo.identifier)) {
            _directionIsReversed = dictionary.value<bool>(ReverseDirectionInfo.identifier);
        }
    }
    if (dictionary.hasKeyAndValue<glm::vec3>(ColorInfo.identifier)) {
        _color = dictionary.value<glm::vec3>(ColorInfo.identifier);
        _color.setViewOption(properties::Property::ViewOptions::Color);
    }
    if (dictionary.hasKeyAndValue<double>(ResolutionInfo.identifier)) {
        _resolution = dictionary.value<double>(ResolutionInfo.identifier);
    }
    if (dictionary.hasKeyAndValue<double>(OpacityInfo.identifier)) {
        _opacity = dictionary.value<double>(OpacityInfo.identifier);
    }
    if (dictionary.hasKeyAndValue<bool>(WireframeInfo.identifier)) {
        _wireframe = dictionary.value<bool>(WireframeInfo.identifier);
    }
    if (dictionary.hasKeyAndValue<double>(RadiusInfo.identifier)) {
        _radius = dictionary.value<double>(RadiusInfo.identifier);
    }

    addProperty(_height);
    addProperty(_radius);
    addProperty(_resolution);
    addProperty(_opacity);
    addProperty(_color);
    addProperty(_wireframe);
}
void RenderableCone::initializeGL() {

    createShaderProgram();

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);
    setRenderBin(Renderable::RenderBin::Overlay);

    // We don't need an index buffer, so we keep it at the default value of 0
    glGenVertexArrays(1, &_lateralSurfaceInfo._vaoID);
    glGenBuffers(1, &_lateralSurfaceInfo._vBufferID);

    glGenVertexArrays(1, &_baseInfo._vaoID);
    glGenBuffers(1, &_baseInfo._vBufferID);

    updateVertexAttributes();
}

void RenderableCone::deinitializeGL() {
    glDeleteVertexArrays(1, &_lateralSurfaceInfo._vaoID);
    glDeleteBuffers(1, &_lateralSurfaceInfo._vBufferID);

    glDeleteVertexArrays(1, &_baseInfo._vaoID);
    glDeleteBuffers(1, &_baseInfo._vBufferID);

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableCone::isReady() const {
    return _programObject != nullptr;
}

// Unbind buffers and arrays
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableCone::updateVertexAttributes() {
    // position attributes
    glVertexAttribPointer(_vaLocVer, _sizeThreeVal, GL_FLOAT, GL_FALSE, 
                        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout),
                        (void*)0);
    glEnableVertexAttribArray(_vaLocVer);
    // color attributes
    glVertexAttribPointer(_vaLocCol, _sizeFourVal, GL_FLOAT, GL_FALSE,
                        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout),
                        (void*)(sizeof(PositionVBOLayout)));
    glEnableVertexAttribArray(_vaLocCol);

    // Update the number of lines to render, same for both vertex arrays
    _count = static_cast<GLsizei>(_vertexLateralSurfaceArray.size() / (_sizeThreeVal + _sizeFourVal));
}
float RenderableCone::calculateBaseRadius()
{
    return _radius * _unit;
}

void RenderableCone::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();
    updateUniforms(data);

    const bool usingFramebufferRenderer =
        global::renderEngine.rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    if (usingFramebufferRenderer) {
        glDepthMask(false);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    //Lateral surface of the cone
    glBindVertexArray(_lateralSurfaceInfo._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _lateralSurfaceInfo._vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexLateralSurfaceArray.size() * sizeof(float),
        _vertexLateralSurfaceArray.data(),
        GL_DYNAMIC_DRAW
    );
    glDisable(GL_CULL_FACE);
    updateVertexAttributes();

    if (_wireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    }
    glDrawArrays(
        GL_TRIANGLE_FAN,
        0,
        _count
    );
    unbindGL();
   
    // Base part of the cone
    if (_showbase) {
        glBindVertexArray(_baseInfo._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _baseInfo._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexBaseArray.size() * sizeof(float),
            _vertexBaseArray.data(),
            GL_STATIC_DRAW
        );
        glDisable(GL_CULL_FACE);

        updateVertexAttributes();
        glDrawArrays(
            GL_TRIANGLE_FAN,
            0,
            _count
        );
    }
    if (_wireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }
    _programObject->deactivate();
}

void RenderableCone::createShaderProgram()
{
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_DSN}/shaders/renderablecone_vs.glsl"),
                absPath("${MODULE_DSN}/shaders/renderablecone_fs.glsl")
            );
        }
    );
}

void RenderableCone::update(const UpdateData& data) {
    _vertexLateralSurfaceArray.clear();
    _vertexBaseArray.clear();
    
    if (_apexIsNodeAttached) {

        if (!global::renderEngine.scene()->sceneGraphNode(_apexNodeId)) {
            LERROR(fmt::format("No scenegraphnode found with id {}", _apexNodeId));
            return;
        }
        _apexPosition = global::renderEngine.scene()->sceneGraphNode(_apexNodeId)->worldPosition();
    }

    if (_baseCenterIsNodeAttached) {

        if (!global::renderEngine.scene()->sceneGraphNode(_baseDirNodeId)) {
            LERROR(fmt::format("No scenegraphnode found with id {}", _baseDirNodeId));
            return;
        }

        glm::dvec3 nodePos = global::renderEngine.scene()->sceneGraphNode(_baseDirNodeId)->worldPosition();

        _baseCenterDirection = glm::normalize(_apexPosition - nodePos);
    }

    _baseVertices.clear();
    int numBaseVertices = _resolution;
    double height = _height * _unit;

    double radius = calculateBaseRadius(); 

    float angleIncrement = glm::radians(360.0 / numBaseVertices);
    glm::dvec3 e0 = glm::normalize(glm::cross(_baseCenterDirection, glm::dvec3(1.0, 0.0, 0.0)));
    glm::dvec3 e1 = glm::normalize(glm::cross(_baseCenterDirection, e0));

    if (_directionIsReversed) {
        _baseCenterPosition = _apexPosition + _baseCenterDirection * height;
    }
    else {
        _baseCenterPosition = _apexPosition - _baseCenterDirection * height;
    }

    for (int i = 0; i < numBaseVertices; ++i) {
        double rad = angleIncrement * i;
        glm::dvec3 p = _baseCenterPosition + (((e0 * glm::cos(rad)) + (e1 * glm::sin(rad))) * radius);
        p = getCoordinatePosFromAnchorNode(p);
        _baseVertices.push_back(p);
    }

    // work around for precision errors
    if (global::navigationHandler.orbitalNavigator().anchorNode()) {
        _localTransform = glm::translate(glm::dmat4(1.0), 
                          global::navigationHandler.orbitalNavigator().anchorNode()->worldPosition());
    }
    _apexPosition = getCoordinatePosFromAnchorNode(_apexPosition);
    _baseCenterPosition = getCoordinatePosFromAnchorNode(_baseCenterPosition);
    
    // upload all positions to the vertex arrays
    fillVertexArrays();

    unbindGL();
}

/*  Returns a position that is relative to the current
    anchor node. This is a method to handle precision
    problems that occur when placing our signal line endings. */
glm::dvec3 RenderableCone::getCoordinatePosFromAnchorNode(glm::dvec3 worldPos) {

    glm::dvec3 anchorNodePos(0);

    if (global::navigationHandler.orbitalNavigator().anchorNode()) {
        anchorNodePos = global::navigationHandler.orbitalNavigator().anchorNode()->worldPosition();
    }

    glm::dvec3 diffPos = glm::dvec3(worldPos.x - anchorNodePos.x, worldPos.y - anchorNodePos.y,
        worldPos.z - anchorNodePos.z);

    return diffPos;
}

void RenderableCone::updateUniforms(const RenderData& data) {
    _programObject->setUniform(_uniformCache.modelView,
        data.camera.combinedViewMatrix() * _localTransform);
    _programObject->setUniform(_uniformCache.projection, data.camera.sgctInternal.projectionMatrix());
}

void RenderableCone::fillVertexArrays() {
    glm::vec3 color = _color;
    glm::vec4 colorAndOpacity = { color, _opacity };
   
    // add base vertices
    if (_showbase) {
        addVertexToVertexArray(_vertexBaseArray, _baseCenterPosition, colorAndOpacity);

        for (int i = 0; i < _baseVertices.size(); ++i) {
            addVertexToVertexArray(_vertexBaseArray, _baseVertices[i], colorAndOpacity);
        }
        addVertexToVertexArray(_vertexBaseArray, _baseVertices[0], colorAndOpacity);
    }
    //add lateral surface vertices
    addVertexToVertexArray(_vertexLateralSurfaceArray, _apexPosition, colorAndOpacity);

    for (int i = 0; i < _baseVertices.size(); ++i) {
        addVertexToVertexArray(_vertexLateralSurfaceArray, _baseVertices[i], colorAndOpacity);
    }
    addVertexToVertexArray(_vertexLateralSurfaceArray, _baseVertices[0], colorAndOpacity);
}

void RenderableCone::addVertexToVertexArray(std::vector<float> &vertexArray,glm::dvec3 position, glm::vec4 color)
{
    vertexArray.push_back(position.x);
    vertexArray.push_back(position.y);
    vertexArray.push_back(position.z);
    vertexArray.push_back(color.r);
    vertexArray.push_back(color.g);
    vertexArray.push_back(color.b);
    vertexArray.push_back(color.a);
}

} // namespace openspace
