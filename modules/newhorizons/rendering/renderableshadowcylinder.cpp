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

#include <modules/newhorizons/rendering/renderableshadowcylinder.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>


namespace {
    const std::string _loggerCat      = "RenderablePlane";
    
    const char* KeyType        = "TerminatorType";
    const char* KeyLightSource = "LightSource";
    const char* KeyObserver    = "Observer";
    const char* KeyBody        = "Body";
    const char* KeyBodyFrame   = "BodyFrame";
    const char* KeyMainFrame   = "MainFrame";
    const char* KeyAberration  = "Aberration";
}

namespace openspace {

RenderableShadowCylinder::RenderableShadowCylinder(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _numberOfPoints("amountOfPoints", "Points", 190, 1, 300)
    , _shadowLength("shadowLength", "Shadow Length", 0.1, 0.0, 0.5)
    , _shadowColor("shadowColor", "Shadow Color",
                   glm::vec4(1.f, 1.f, 1.f, 0.25f), glm::vec4(0.f), glm::vec4(1.f))
    , _shader(nullptr)
    , _vao(0)
    , _vbo(0)
{
    addProperty(_numberOfPoints);
    addProperty(_shadowLength);
    addProperty(_shadowColor);

    _terminatorType = dictionary.value<std::string>(KeyType);
    _lightSource = dictionary.value<std::string>(KeyLightSource);
    _observer = dictionary.value<std::string>(KeyObserver);
    _body = dictionary.value<std::string>(KeyBody);
    _bodyFrame = dictionary.value<std::string>(KeyBodyFrame);
    _mainFrame = dictionary.value<std::string>(KeyMainFrame);
    
    _aberration = SpiceManager::AberrationCorrection(dictionary.value<std::string>(KeyAberration));
}

bool RenderableShadowCylinder::initialize() {
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram(
        "ShadowProgram",
        "${MODULE_NEWHORIZONS}/shaders/terminatorshadow_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/terminatorshadow_fs.glsl"
    );

    if (!_shader)
        return false;


    return true;
}

bool RenderableShadowCylinder::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    return true;
}

bool RenderableShadowCylinder::isReady() const {
    return true;
}
    
void RenderableShadowCylinder::render(const RenderData& data){
    glDepthMask(false);
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    //_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    //_shader->setUniform("ModelTransform", glm::mat4(_stateMatrix));


    _shader->setUniform("shadowColor", _shadowColor);
    //setPscUniforms(*_shader.get(), data.camera, data.position);
    
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, static_cast<GLsizei>(_vertices.size()));
    glBindVertexArray(0);

    _shader->deactivate();

    glDepthMask(true);
}

void RenderableShadowCylinder::update(const UpdateData& data) {
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_bodyFrame, _mainFrame, data.time);
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }
    createCylinder(data.time);
}

glm::vec4 psc_addition(glm::vec4 v1, glm::vec4 v2) {
    float k = 10.f;
    float ds = v2.w - v1.w;
    if (ds >= 0) {
        float p = pow(k, -ds);
        return glm::vec4(v1.x*p + v2.x, v1.y*p + v2.y, v1.z*p + v2.z, v2.w);
    }
    else {
        float p = pow(k, ds);
        return glm::vec4(v1.x + v2.x*p, v1.y + v2.y*p, v1.z + v2.z*p, v1.w);
    }
}

void RenderableShadowCylinder::createCylinder(double time) {
    auto res = SpiceManager::ref().terminatorEllipse(
        _body,
        _observer,
        _bodyFrame,
        _lightSource,
        SpiceManager::terminatorTypeFromString(_terminatorType),
        _aberration,
        time,
        _numberOfPoints
    );
    
    std::vector<psc> terminatorPoints;
    std::transform(
        res.terminatorPoints.begin(),
        res.terminatorPoints.end(),
        std::back_inserter(terminatorPoints),
        [](const glm::dvec3& p) {
            PowerScaledCoordinate psc = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
            psc[3] += 3;
            return psc;
        }
    );
    
    double lt;
    glm::dvec3 vecLightSource =
        SpiceManager::ref().targetPosition(_body, _lightSource, _mainFrame, _aberration, time, lt);

    vecLightSource = glm::inverse(_stateMatrix) * vecLightSource;

    vecLightSource *= _shadowLength;
    _vertices.clear();

    psc endpoint = psc::CreatePowerScaledCoordinate(vecLightSource.x, vecLightSource.y, vecLightSource.z);
    for (const auto& v : terminatorPoints) {
        _vertices.push_back({ v[0], v[1], v[2], v[3] });
        glm::vec4 f = psc_addition(v.vec4(), endpoint.vec4());
        _vertices.push_back({ f[0], f[1], f[2], f[3] });
    }
    _vertices.push_back(_vertices[0]);
    _vertices.push_back(_vertices[1]);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, _vertices.size() * sizeof(CylinderVBOLayout), NULL, GL_DYNAMIC_DRAW);
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertices.size() * sizeof(CylinderVBOLayout), &_vertices[0]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
    glBindVertexArray(0);
}

} // namespace openspace
