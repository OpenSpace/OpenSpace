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
#include <modules/dsn/rendering/renderablestationfov.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <modules/base/basemodule.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>

namespace {
    constexpr const char* ProgramName = "StationFovProgram";
    constexpr const char* _loggerCat = "RenderableStationFov";

    constexpr const std::array <const char*, openspace::RenderableCone::uniformCacheSize> UniformNames = {
    "modelView", "projectionTransform" };

    constexpr openspace::properties::Property::PropertyInfo ViewAngleInfo = {
        "ViewAngle",
        "View Angle",
        "Field of view angle"
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceFadeInfo = {
        "DistanceFade",
        "Distance Fade",
        "Fade applied linearly from viewpoint"
    };
} // namespace

namespace openspace {


documentation::Documentation RenderableStationFov::Documentation() {
    using namespace documentation;

    documentation::Documentation doc{
            "Renderable Station Fov",
            "dsn_renderable_renderablestationfov",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderableStationFov"),
                    Optional::No
                },
                {
                    ViewAngleInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    ViewAngleInfo.description
                },
                {
                    DistanceFadeInfo.identifier,
                    new BoolVerifier,
                    Optional::Yes,
                    ViewAngleInfo.description
                }
            }
    };

    // @TODO cleanup
    // Insert the parents documentation entries until we have a verifier that can deal
    // with class hierarchy
    documentation::Documentation parentDoc = RenderableCone::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );

    return doc;
}

RenderableStationFov::RenderableStationFov(const ghoul::Dictionary& dictionary)
    : RenderableCone(dictionary)
    , _angle(ViewAngleInfo, 160.0, 0.0, 180.0)
    , _distanceFade(DistanceFadeInfo, true)
{
    _showbase = false;
    _directionIsReversed = true;
    _wireframe = false;

    if (dictionary.hasKeyAndValue<double>(ViewAngleInfo.identifier)) {
        _angle = dictionary.value<double>(ViewAngleInfo.identifier);
    } 
    addProperty(_distanceFade);
    addProperty(_angle);
    removeProperty(_radius);
}

void RenderableStationFov::createShaderProgram()
{
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_DSN}/shaders/renderablestationfov_vs.glsl"),
                absPath("${MODULE_DSN}/shaders/renderablestationfov_fs.glsl")
            );
        }
    );
}

void RenderableStationFov::updateVertexAttributes()
{
    // position attributes
    glVertexAttribPointer(_vaLocVer, _sizeThreeVal, GL_FLOAT, GL_FALSE,
        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout) + sizeof(float),
        (void*)0);
    glEnableVertexAttribArray(_vaLocVer);
    // color attributes
    glVertexAttribPointer(_vaLocCol, _sizeFourVal, GL_FLOAT, GL_FALSE,
        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout) + sizeof(float),
        (void*)(sizeof(PositionVBOLayout)));
    glEnableVertexAttribArray(_vaLocCol); 

    // distance from apex attribute
    glVertexAttribPointer(_vaLocDist, _sizeOneVal, GL_FLOAT, GL_FALSE,
        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout) + sizeof(float),
        (void*)(sizeof(PositionVBOLayout) + sizeof(ColorVBOLayout)));
    glEnableVertexAttribArray(_vaLocDist);

    // Update the number of lines to render, same for both vertex arrays
    _count = static_cast<GLsizei>(_vertexLateralSurfaceArray.size() / (_sizeThreeVal + _sizeFourVal + _sizeOneVal));
}

void RenderableStationFov::fillVertexArrays()
{
    glm::vec4 colorAndOpacity = { glm::vec3(_color), _opacity };

    float apexFade = 1.0;
    float baseVerticeFade = 1.0;
    if(_distanceFade) {
        baseVerticeFade = 0.0;
    }
    // add base vertices
    //addVertexToVertexArray(_vertexBaseArray, _baseCenterPosition, colorAndOpacity, apexFade);

    //for (int i = 0; i < _baseVertices.size(); ++i) {
    //    addVertexToVertexArray(_vertexBaseArray, _baseVertices[i], colorAndOpacity, baseVerticeFade);
    //}
    //addVertexToVertexArray(_vertexBaseArray, _baseVertices[0], colorAndOpacity, baseVerticeFade);

    //add lateral surface vertices
    addVertexToVertexArray(_vertexLateralSurfaceArray, _apexPosition, colorAndOpacity, apexFade);

    for (int i = 0; i < _baseVertices.size(); ++i) {
        addVertexToVertexArray(_vertexLateralSurfaceArray, _baseVertices[i], colorAndOpacity, baseVerticeFade);
    }
    addVertexToVertexArray(_vertexLateralSurfaceArray, _baseVertices[0], colorAndOpacity, baseVerticeFade);

}

void RenderableStationFov::addVertexToVertexArray(std::vector<float>& vertexArray, glm::dvec3 position, glm::vec4 color, float distanceFade)
{
    vertexArray.push_back(position.x);
    vertexArray.push_back(position.y);
    vertexArray.push_back(position.z);
    vertexArray.push_back(color.r);
    vertexArray.push_back(color.g);
    vertexArray.push_back(color.b);
    vertexArray.push_back(color.a);
    vertexArray.push_back(distanceFade);
}

float RenderableStationFov::calculateBaseRadius()
{
    double angle = glm::radians(float(_angle));
    angle = angle / 2.0; //Half of the full cone angle to get a right -angled triangle

    float radius = _height * _unit * tan(angle);

    return radius;
}

} // namespace openspace
