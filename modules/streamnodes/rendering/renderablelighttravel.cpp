/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <modules/streamnodes/rendering/renderablelighttravel.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>

// Test debugging tools more then logmanager
//#include <ghoul/logging/logmanager.h>
//#include <ghoul/logging/consolelog.h>
//#include <ghoul/logging/visualstudiooutputlog.h>

//#include <thread>
#include <openspace/query/query.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/io/texture/texturereader.h>

namespace {
    constexpr const char* _loggerCat = "renderableLightTravel";

    constexpr openspace::properties::Property::PropertyInfo LightSpeedInfo = {
        "lightSpeed",
        "Speed of light",
        "The speed of light."
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "lineWidth",
        "Line width",
        "This value specifies the line width of the field lines if the "
        "selected rendering method includes lines."
    };
    constexpr openspace::properties::Property::PropertyInfo RenderModeInfo = {
        "renderMode",
        "The draw method",
        "Can be used to decide what rendering method to use."
    };
    constexpr openspace::properties::Property::PropertyInfo LightColorInfo = {
        "lightColor",
        "The color of the light particle",
        "Choose what color to light the particle as it is traversing the line."
    };
    constexpr openspace::properties::Property::PropertyInfo DefaultcolorInfo = {
        "defaultColor",
        "The color of the lines",
        "Choose what color each line should have as default value."
    };
    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "pointSize",
        "Size of points",
        "Change the size of the points."
    };
    constexpr openspace::properties::Property::PropertyInfo TimeStepInfo = {
        "timeStep",
        "Timestep for light travel",
        "Change the timestep for the points along the line between sun and earth."
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceFactorInfo = {
        "distanceFactor",
        "The distance factor for what to show as default vs light",
        "This value is multiplicated by a maximum distance of 1000000000.f meters."
    };

}

namespace openspace {
    using namespace properties;
    RenderableLightTravel::RenderableLightTravel(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _pLightSpeed(LightSpeedInfo, 299792458.f, 0, 299792458.f)
        , _pLineWidth(LineWidthInfo, 5, 0.1, 20)
        , _pRenderMode(RenderModeInfo, OptionProperty::DisplayType::Dropdown)
        , _pDefaultColor(DefaultcolorInfo, glm::vec4(0.3, 0.3, 0.3, 0),
            glm::vec4(0.f),
            glm::vec4(1.f))
        , _pLightColor(LightColorInfo, glm::vec4(1, 1, 1, 1),
            glm::vec4(0.f),
            glm::vec4(1.f))
        , _pPointSize(PointSizeInfo, 2.f, 0, 20)
        , _pTimeStep(TimeStepInfo, 1, 1, 30)
        , _pDistanceFactor(DistanceFactorInfo, 20, 1, 30)
    {
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
    }

void RenderableLightTravel::initializeGL() {
    _dictionary.reset();
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "Lighttravel",
        absPath("${MODULE_STREAMNODES}/shaders/lighttravel_vs.glsl"),
        absPath("${MODULE_STREAMNODES}/shaders/lighttravel_fs.glsl")
    );

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
    glm::vec3 currentpos = glm::vec3(0.0, 0.0, 0.0);
    addProperty(_pLightSpeed);
    addProperty(_pLineWidth);
    addProperty(_pDistanceFactor);

    _pLightSpeed = 299792458.f;
   
    _triggerTime = -1;        
    //Earthnode worldposition, is not aligned with the actual position shown as it seems right now.
       
    _spriteTexture = nullptr;
    std::string texturepath = absPath("${SYNC}/http/stars_textures/1/halo.png");
        
    _spriteTexture = ghoul::io::TextureReader::ref().loadTexture(
        texturepath
    );
    if (_spriteTexture) {
        _spriteTexture->uploadTexture();
    }

    _pRenderMode.addOption(static_cast<int>(RenderMethod::LineStrip), "LineStrip");
    _pRenderMode.addOption(static_cast<int>(RenderMethod::Lines), "Lines");
    _pRenderMode.addOption(static_cast<int>(RenderMethod::Points), "Points");
    _pRenderMode.addOption(static_cast<int>(RenderMethod::Sprites), "Sprites");
    addProperty(_pRenderMode);
    addProperty(_pLightColor);
    addProperty(_pDefaultColor);
    addProperty(_pPointSize);
    addProperty(_pTimeStep);

}
void RenderableLightTravel::deinitializeGL()
{
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}
double RenderableLightTravel::calculateEndTime(const double starttime, 
    const glm::vec3 startpos, const glm::vec3 endpos) {

    glm::vec3 newpos = glm::vec3(0, 0, 0);
    glm::vec3 normalizedVector = glm::normalize(endpos);
    _normalizedVector = normalizedVector;
    double endtime = starttime;
    int counter = 0;
    positions.clear();
    positions.push_back(startpos);
    glm::vec3 newpos2 = glm::vec3(0, 0, 0);
    newpos2.x = normalizedVector.x * _Timesincestart * _pLightSpeed;
    newpos2.y = normalizedVector.y * _Timesincestart * _pLightSpeed;
    newpos2.z = normalizedVector.z * _Timesincestart * _pLightSpeed;
    int interval = _pTimeStep;

    while(endtime - starttime < 500){
        newpos.x += interval * _pLightSpeed * normalizedVector.x;
        newpos.y += interval * _pLightSpeed * normalizedVector.y;
        newpos.z += interval * _pLightSpeed * normalizedVector.z;

        endtime += interval;
        ++counter;

        if (newpos.x > endpos.x) {
            newpos = endpos;
            positions.push_back(newpos);
            break;
        }
        positions.push_back(newpos);
    }
    positions.push_back(endpos);
       
    _needPositionUpdate = true;
    return endtime;
}
bool RenderableLightTravel::isReady() const
{
    return _shaderProgram != nullptr;
}
void RenderableLightTravel::render(const RenderData& data, RendererTasks& rendererTask)
{
    if (!this->_enabled) return;
    if (_triggerTime == -1) {
        _triggerTime = data.time.j2000Seconds();
        _endTime = _triggerTime + 599;
    }
       
    _shaderProgram->activate();
    // Calculate Model View MatrixProjection

    const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
    const glm::dmat4 modelMat =
    glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
    rotMat *
    glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
    const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

    _shaderProgram->setUniform("modelViewProjection",
        data.camera.sgctInternal.projectionMatrix()* glm::mat4(modelViewMat));

    glm::dmat4 modelMatrix =
    glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
    glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
    glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::mat4 projectionMatrix = data.camera.projectionMatrix();

    glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

    glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
    glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        glm::dvec3 otherVector(
            cameraUpDirectionWorld.y,
            cameraUpDirectionWorld.x,
            cameraUpDirectionWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

    glBindVertexArray(_vertexArrayObject);

    if (positions.size() > 2) {
        const double currentTime = data.time.j2000Seconds();
        _Timesincestart = currentTime - _triggerTime;
        float dist_from_start = glm::distance(positions[0], positions[positions.size()]);
        float transmissiontime = _endTime - _triggerTime;
        _shaderProgram->setUniform("in_time_since_start", _Timesincestart);
        _shaderProgram->setUniform("normalizedVectorFromSunToEarth", _normalizedVector);
        _shaderProgram->setUniform("renderMode", _pRenderMode);
        _shaderProgram->setUniform("pointSize", _pPointSize);
        _shaderProgram->setUniform("defaultColor", _pDefaultColor);
        _shaderProgram->setUniform("lightColor", _pLightColor);
        _shaderProgram->setUniform("distanceFactor", _pDistanceFactor);

    }

    if(_pRenderMode == 0){
        glLineWidth(_pLineWidth);
        GLint temp = 0;
        glDrawArrays(
            GL_LINE_STRIP,
            temp,
            static_cast<GLsizei>(positions.size())
        );
    }
    else if(_pRenderMode == 1){
        glLineWidth(_pLineWidth);
        GLint temp = 0;
        glDrawArrays(
            GL_LINES,
            temp,
            static_cast<GLsizei>(positions.size())
        );
    }
    else if(_pRenderMode == 2){
        GLint temp = 0;
        glEnable(GL_PROGRAM_POINT_SIZE);
        glDrawArrays(
            GL_POINTS,
            temp,
            static_cast<GLsizei>(positions.size())
        );
    }
    else if(_pRenderMode == 3){
        ghoul::opengl::TextureUnit spriteTextureUnit;
        spriteTextureUnit.activate();
        _spriteTexture->bind();
        _shaderProgram->setUniform("spriteTexture", spriteTextureUnit);
        glEnable(GL_PROGRAM_POINT_SIZE);

        glDrawArrays(
            GL_POINTS,
            0,
            static_cast<GLsizei>(positions.size())
        );
    }
    glBindVertexArray(0);
    _shaderProgram->deactivate();
}

inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableLightTravel::update(const UpdateData& data)
{
    if (!this->_enabled) return;
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }

    const double currentTime = data.time.j2000Seconds();

    if (_needPositionUpdate) {
        SceneGraphNode* earthNode = sceneGraphNode("Earth");
        glm::vec3 earthPos = earthNode->worldPosition();
        glm::vec3 currentpos = glm::vec3(0.0, 0.0, 0.0);

        _needPositionUpdate = false;
        _endTime = calculateEndTime(_triggerTime, currentpos, earthPos);

    }
    if (_triggerTime < currentTime && _endTime > currentTime) {

        glBindVertexArray(_vertexArrayObject);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        const std::vector<glm::vec3>& vertPos = positions;

        glBufferData(
            GL_ARRAY_BUFFER,
            vertPos.size() * sizeof(glm::vec3),
            vertPos.data(),
            GL_STATIC_DRAW
        );
        constexpr const GLuint VaPosition = 0;
        glEnableVertexAttribArray(VaPosition);
        glVertexAttribPointer(VaPosition, 3, GL_FLOAT, GL_FALSE, 0, 0);

        constexpr const GLuint VaDistance = 1;
        constexpr const GLuint VaTimeSinceStart = 2;
        constexpr const GLuint VaTransmissionTime = 3;
        constexpr const GLuint VaLightTravelTime = 4;

        //glEnable(GL_PROGRAM_POINT_SIZE);

        glVertexAttribPointer(VaDistance, 1, GL_FLOAT, GL_FALSE, 0, 0);
        glEnableVertexAttribArray(VaDistance);

        glVertexAttribPointer(VaTimeSinceStart, 1, GL_FLOAT, GL_FALSE, 0, 0);
        glEnableVertexAttribArray(VaTimeSinceStart);

        glVertexAttribPointer(VaTransmissionTime, 1, GL_FLOAT, GL_FALSE, 0, 0);
        glEnableVertexAttribArray(VaTransmissionTime);

        glVertexAttribPointer(VaLightTravelTime, 1, GL_FLOAT, GL_FALSE, 0, 0);
        glEnableVertexAttribArray(VaLightTravelTime);

        unbindGL();
    }
    else {
        _needPositionUpdate = true;
        _triggerTime = data.time.j2000Seconds();
    }
}
}
