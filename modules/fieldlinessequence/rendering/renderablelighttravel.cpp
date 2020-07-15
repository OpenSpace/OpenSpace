/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
 //including our own h file
#include <modules/fieldlinessequence/rendering/renderablelighttravel.h>

// Includes from fieldlinessequence, might not need all of them
#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
// Test debugging tools more then logmanager
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/filesystem/cachemanager.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <thread>
#include <openspace/json.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>


namespace {
    constexpr const char* _loggerCat = "renderableLightTravel";

    constexpr openspace::properties::Property::PropertyInfo LightSpeedInfo = {
       "lightspeed",
       "speed of light",
       "The speed of light"
    };

}

namespace openspace {
    using namespace properties;
    RenderableLightTravel::RenderableLightTravel(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _lightspeed(LightSpeedInfo, 3000000000)

    {
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
    }

    void RenderableLightTravel::initializeGL() {

        _dictionary.reset();
        _shaderProgram = global::renderEngine.buildRenderProgram(
            "Lighttravel",
            absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/lighttravel_vs.glsl"),
            absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/lighttravel_fs.glsl")
        );

        glGenVertexArrays(1, &_vertexArrayObject);
        glGenBuffers(1, &_vertexPositionBuffer);
        

        setRenderBin(Renderable::RenderBin::Transparent);
        glm::vec3 currentpos = glm::vec3(0.0, 0.0, 0.0);
        //positions.push_back(currentpos);
        addProperty(_lightspeed);
        //_lightspeed = 300 * 10e6;
        _lightspeed = 299792458.f;
        SceneGraphNode* earthNode = sceneGraphNode("Earth");
       // glm::vec3 earthPos = earthNode->worldPosition();
        glm::vec3 earthPos = glm::vec3(94499869340, -115427843118, 11212075887.3);
        glm::vec3 earthToSun = glm::vec3(earthPos);
        glm::vec3 newpos = glm::vec3(0.0, 0.0, 0.0);
        /// should probably be distance from points to earth
        /*
        while(currentpos.x < 94499869340){
            newpos = currentpos + (_lightspeed / glm::length(earthToSun)) * earthToSun;
            positions.push_back(newpos);
            earthToSun = earthPos - newpos;
            currentpos = newpos;
        }
        */
        //positions.push_back(glm::vec3(94499869340 / 2, -115427843118 / 2, 11212075887.3 / 2));
       // positions.push_back(earthPos);

        //_triggerTime = Time::convertTime("2000-07-14T10:03:44.00");
        _triggerTime = -1;
        //LDEBUG("_triggertime: " + std::to_string(_triggerTime));
        //_endTime = _triggerTime + 599;
        
        //Earthnode worldposition, is not aligned with the actual position shown as it seems right now.
       
        //glm::vec3 earthPos = earthNode->position();
       // positions.push_back(earthPos);
       // positions.push_back(earthPos);

        LDEBUG("vi kom in i init");

   

    }
    void RenderableLightTravel::deinitializeGL()
    {
        glDeleteVertexArrays(1, &_vertexArrayObject);
        _vertexArrayObject = 0;

        glDeleteBuffers(1, &_vertexPositionBuffer);
        _vertexPositionBuffer = 0;

        if (_shaderProgram) {
            global::renderEngine.removeRenderProgram(_shaderProgram.get());
            _shaderProgram = nullptr;
        }


    }
     double RenderableLightTravel::calculateEndTime(const double starttime, const glm::vec3 startpos, const glm::vec3 endpos) {
        glm::vec3 newpos = glm::vec3(0, 0, 0);
        glm::vec3 normalizedVector = glm::normalize(endpos);
        _normalizedVector = normalizedVector;
        double endtime = starttime;
        LDEBUG("distance" + std::to_string(glm::distance(newpos, endpos)));
        int counter = 0;
        float lightspeed = 299792458;
        positions.clear();
        positions.push_back(startpos);
        //while (glm::distance(newpos, endpos) > 100000) {
        while(endtime - starttime < 500){
            newpos.x += 20 * lightspeed * normalizedVector.x;
            newpos.y += 20 * lightspeed * normalizedVector.y;
            newpos.z += 20 * lightspeed * normalizedVector.z;
           
            
            endtime += 20;
            ++counter;
            //LDEBUG("newpos.y" + std::to_string(newpos.y));
            //LDEBUG("newpos.x" + std::to_string(newpos.x));
            //LDEBUG("endtime" + std::to_string(endtime));
            if (newpos.x > endpos.x) {
                newpos = endpos;
                positions.push_back(newpos);
                break;
            }
            positions.push_back(newpos);
        }
        positions.push_back(endpos);
        LDEBUG("endtime" + std::to_string(endtime));
        LDEBUG("newpos.y" + std::to_string(newpos.y));
        LDEBUG("newpos.x" + std::to_string(newpos.x));
        LDEBUG("position size" + std::to_string(positions.size()));
        return endtime;
    }
    bool RenderableLightTravel::isReady() const
    {
        return _shaderProgram != nullptr;
    }
    void RenderableLightTravel::render(const RenderData& data, RendererTasks& rendererTask)
    {
        
        if (_triggerTime == -1) {
            _triggerTime = data.time.j2000Seconds();
            _endTime = _triggerTime + 599;
        }
       
        
        
       
        
        
        _shaderProgram->activate();
        //LDEBUG("vi kom in i render");
        // Calculate Model View MatrixProjection
        const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
        const glm::dmat4 modelMat =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            rotMat *
            glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
        const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

        _shaderProgram->setUniform("modelViewProjection",
            data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat));
        glBindVertexArray(_vertexArrayObject);
        

        if(positions.size() > 2){
        const double currentTime = data.time.j2000Seconds();
        float timeSinceStart = currentTime - _triggerTime;
        float dist_from_start = glm::distance(positions[0], positions[positions.size()]);
        float transmissiontime = _endTime - _triggerTime;
        _shaderProgram->setUniform("in_time_since_start", timeSinceStart);
        _shaderProgram->setUniform("in_dist_from_start", dist_from_start);
        _shaderProgram->setUniform("in_transmission_time", transmissiontime);
        _shaderProgram->setUniform("in_light_travel_time", timeSinceStart);
        _shaderProgram->setUniform("normalizedvectorFromSuntoEarth", _normalizedVector);
        }

        glLineWidth(5.0f);
        GLint temp = 0;
        glDrawArrays(
            GL_LINE_STRIP,
            //GL_LINES,
            temp,
            static_cast<GLsizei>(positions.size())
        );
        glBindVertexArray(0);
        _shaderProgram->deactivate();

        

    }
    inline void unbindGL() {
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }
    void RenderableLightTravel::update(const UpdateData& data)
    {
         if (_shaderProgram->isDirty()) {
              _shaderProgram->rebuildFromFile();
         }
         /*
         if (_Timesincestart == -1) {
             _Timesincestart = 
        }
        */
         const double currentTime = data.time.j2000Seconds();
         _needPositionUpdate = true;
         if (_needPositionUpdate) {
             SceneGraphNode* earthNode = sceneGraphNode("Earth");
             glm::vec3 earthPos = earthNode->worldPosition();
             glm::vec3 currentpos = glm::vec3(0.0, 0.0, 0.0);
             /*
             positions.push_back(currentpos);
             positions.push_back(earthPos);
             _needPositionUpdate = false;
             */
             _endTime = calculateEndTime(_triggerTime, currentpos, earthPos);
             _needPositionUpdate = false;
         }
         if (_triggerTime < currentTime && _endTime > currentTime) {
         
         
         // glm::vec3 earthPos = 
         //     global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
         //positions.push_back(earthPos);
         //_endTime = calculateEndTime(_triggerTime, currentpos, earthPos);

        //LDEBUG("position size:" + std::to_string(positions.size()));
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
       
            /*
         glVertexAttribPointer(VaDistance, 1, GL_FLOAT, GL_FALSE, 0, 0);
         glEnableVertexAttribArray(VaDistance);

         glVertexAttribPointer(VaTimeSinceStart, 1, GL_FLOAT, GL_FALSE, 0, 0);
         glEnableVertexAttribArray(VaTimeSinceStart);

         glVertexAttribPointer(VaTransmissionTime, 1, GL_FLOAT, GL_FALSE, 0, 0);
         glEnableVertexAttribArray(VaTransmissionTime);

         glVertexAttribPointer(VaLightTravelTime, 1, GL_FLOAT, GL_FALSE, 0, 0);
         glEnableVertexAttribArray(VaLightTravelTime);
         */







         unbindGL();
         }
         else {
             _needPositionUpdate = true;
             _triggerTime = data.time.j2000Seconds();
         }
    }
}
