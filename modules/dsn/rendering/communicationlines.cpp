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

#include <modules/dsn/rendering/communicationlines.h>
#include <openspace/scene/scene.h>

#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/camera.h>

namespace openspace {
    constexpr const char* _loggerCat = "CommunicationLine";

    //Member functions
    CommunicationLines::CommunicationLines(const ghoul::Dictionary& dictionary)
        :RenderableCommunicationPackage(dictionary){
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
        extractData();
    }

    void CommunicationLines::extractData() {
       
        if (!DsnManager::extractMandatoryInfoFromDictionary(_identifier, _dictionary)){
            LERROR(fmt::format("{}: Did not manage to extract data.", _identifier));
        }
        else {
            LDEBUG(fmt::format("{}: Successfully read data.", _identifier));
        }
   
    }
    
    void CommunicationLines::initializeGL() {
        RenderableCommunicationPackage::initializeGL();

        // We don't need an index buffer, so we keep it at the default value of 0
        glGenVertexArrays(1, &_lineRenderInformation._vaoID);
        glGenBuffers(1, &_lineRenderInformation._vBufferID);
    }
    void CommunicationLines::deinitializeGL(){
        glDeleteVertexArrays(1, &_lineRenderInformation._vaoID);
        glDeleteBuffers(1, &_lineRenderInformation._vBufferID);


        RenderableCommunicationPackage::deinitializeGL();
    }
    void CommunicationLines::update(const UpdateData& data){

        //Update vertex array with values from data 
        Signal exampleSignal;
        exampleSignal.spacecraft = "Voyager_1";
        exampleSignal.station = "DSS54";
        exampleSignal.color = GetSiteColor(exampleSignal.station);
        _lineColor = exampleSignal.color;

        Signal exampleSignal2;
        exampleSignal2.spacecraft = "Voyager_2";
        exampleSignal2.station = "DSS14";
        exampleSignal2.color = GetSiteColor(exampleSignal2.station);
        _lineColor = exampleSignal2.color;

        DsnData signalDataVector;
        signalDataVector.signals.push_back(exampleSignal);
        signalDataVector.signals.push_back(exampleSignal2);

        // Make space for the vertices
        _vertexArray.clear();

        for (int i = 0; i < signalDataVector.signals.size(); i++) {
               
            Signal currentSignal = signalDataVector.signals[i];

            _vertexArray.push_back(getPositionForGeocentricSceneGraphNode(currentSignal.station));
            _vertexArray.push_back(getSuitablePrecisionPositionForSceneGraphNode(currentSignal.spacecraft));
        }

     
        // ... and upload them to the GPU
        glBindVertexArray(_lineRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _lineRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexArray.size() * sizeof(LineVBOLayout),
            _vertexArray.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

        // Directly render the entire step
        _lineRenderInformation.first = 0;
        _lineRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
     
        glBindVertexArray(0);
    }


    //Our spacecrafts are often very far from Earth (and the sun) which gives precision errors when getting close
    //To correct this we change the reference coordinate system when rendering these points if we want to zoom in on them
    RenderableCommunicationPackage::LineVBOLayout CommunicationLines::getSuitablePrecisionPositionForSceneGraphNode(std::string id) {
        
        SceneGraphNode* focusNode = global::navigationHandler.focusNode();
        RenderableCommunicationPackage::LineVBOLayout position;
        glm::vec3 nodePos;

        if (focusNode->identifier() == id) {
            //make high precision calculations
            LDEBUG("IdentfierNode");
            Camera* cam = global::renderEngine.scene()->camera();

            glm::dvec3 cameraPos = cam->positionVec3();
            glm::dvec3 focusNodePos = focusNode->worldPosition();

            glm::vec3 diff = glm::vec3(cameraPos.x - focusNodePos.x, cameraPos.y - focusNodePos.y, cameraPos.z - focusNodePos.z);
            float distanceCameraToFocus = glm::sqrt(glm::pow(diff.x,2.0) + glm::pow(diff.y, 2.0)+ glm::pow(diff.z, 2.0));
            float distanceToSwap = 60000000;

            if (distanceCameraToFocus < distanceToSwap) {
                LDEBUG(fmt::format("Distance: {}", std::to_string(distanceCameraToFocus)));
                glm::dvec3 earthPos(global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition());
                // And get the current location of the object
                const glm::dvec3 spacecraftPosWorld = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();
                const glm::dvec3 v1 = { spacecraftPosWorld.x, spacecraftPosWorld.y, spacecraftPosWorld.z };

                nodePos = {0.0,0.0,0.0};
                _lineRenderInformation._localTransformSpacecraft = glm::translate(glm::dmat4(1.0), v1);
            }else{
                nodePos = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();
            }
        }else {
            nodePos = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();
        }
        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;
       
        return position;
    }

    //Since our station dishes have a static translation from earth, we can get their local translation
    RenderableCommunicationPackage::LineVBOLayout CommunicationLines::getPositionForGeocentricSceneGraphNode(const char* id) {

        glm::vec3 nodePos = global::renderEngine.scene()->sceneGraphNode(id)->position();
        RenderableCommunicationPackage::LineVBOLayout position;
        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;

        return position;
    }

} // namespace openspace


