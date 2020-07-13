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

        positions.push_back(glm::vec3(0.0, 0.0, 0.0));
        glm::vec3 earthPos = glm::vec3(94499869340, -115427843118, 11212075887.3);
        //positions.push_back(glm::vec3(94499869340 / 2, -115427843118 / 2, 11212075887.3 / 2));
        positions.push_back(earthPos);
        //Earthnode worldposition, is not aligned with the actual position shown as it seems right now.
        //SceneGraphNode* earthNode = sceneGraphNode("Earth");
        //glm::vec3 earthPos = earthNode->worldPosition();
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
    bool RenderableLightTravel::isReady() const
    {
        return _shaderProgram != nullptr;
    }
    void RenderableLightTravel::render(const RenderData& data, RendererTasks& rendererTask)
    {
        /*
        if(positions.size() < 3){
        SceneGraphNode* earthNode = sceneGraphNode("Earth");
        glm::vec3 earthPos = earthNode->worldPosition();
       // glm::vec3 earthPos = 
       //     global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
        positions.push_back(earthPos);
        }
        */
        
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

        glLineWidth(5.0f);
        GLint temp = 0;
        glDrawArrays(
            GL_LINE_STRIP,
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
         glEnable(GL_PROGRAM_POINT_SIZE);
         glVertexAttribPointer(VaPosition, 3, GL_FLOAT, GL_FALSE, 0, 0);


         unbindGL();
    }
}
