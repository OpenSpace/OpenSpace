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

    bool CommunicationLines::checkSignal(double currentTime, std::string signalStartTime, std::string signalEndTime) {
           double startTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalStartTime);
           double endTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalEndTime);

           if (startTimeInSeconds <= currentTime && endTimeInSeconds >= currentTime)
               return true;

       return false;
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

    void CommunicationLines::update(const UpdateData& data) {

        DsnManager::DsnData signalDataVector = DsnManager::_dsnData;
        double currentTime = data.time.j2000Seconds();

        signalDataVector.signals[1].color = GetSiteColor(signalDataVector.signals[1].dishName);
        _lineColor = signalDataVector.signals[1].color; 

        // Make space for the vertices
        _vertexArray.clear();

        //update focusnode information
        _focusNode = global::navigationHandler.focusNode();
        _lineRenderInformation._localTransformSpacecraft = glm::translate(glm::dmat4(1.0), _focusNode->worldPosition());

        //fill vertexarray with signal position data
        //Need to change this later, it's to slow.
        for (int i = 0; i < signalDataVector.signals.size(); i++) { 
            DsnManager::Signal currentSignal = signalDataVector.signals[i];

            if (checkSignal(currentTime, DsnManager::_dsnData.signals[i].startTime, DsnManager::_dsnData.signals[i].endTime))
            {
               _vertexArray.push_back(getPositionForGeocentricSceneGraphNode(currentSignal.dishName.c_str()));
               _vertexArray.push_back(getSuitablePrecisionPositionForSceneGraphNode(currentSignal.spacecraft.c_str()));
            }
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
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr); // the vertex data points for all positions

        // Directly render the entire step
        _lineRenderInformation.first = 0;
        _lineRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
     
        glBindVertexArray(0);
        
    }

    /*Returns a position calculated where the focusNode position is origin(0,0,0) */
    glm::dvec3 CommunicationLines::GetCoordinatePosFromFocusNode(SceneGraphNode* node) {

        glm::dvec3 nodePos = node->worldPosition();
        glm::dvec3 focusNodePos = _focusNode->worldPosition();

        glm::dvec3 diff = glm::vec3(nodePos.x - focusNodePos.x, nodePos.y - focusNodePos.y,
            nodePos.z - focusNodePos.z);

        return diff;
    }

    /* Our spacecrafts are often very far from Earth (and the sun) which gives precision 
    * errors when we getting close to the node. To correct this we make a localtransform
    * to the current focusNode and calculate positions with the focusNode as origin. This
    * gives us a exact renderposition when we get closer to spacecrafts */
    RenderableCommunicationPackage::LineVBOLayout 
        CommunicationLines::getSuitablePrecisionPositionForSceneGraphNode(std::string id) {
        
        RenderableCommunicationPackage::LineVBOLayout position;

        SceneGraphNode* spacecraftNode = global::renderEngine.scene()->sceneGraphNode(id);
        glm::dvec3 nodePos = GetCoordinatePosFromFocusNode(spacecraftNode);

        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;
       
        return position;
    }

    /* Since our station dishes have a static translation from Earth, we can get their
    * local translation the reason to have a separate  modeltransform is to keep an
    * exact render position even when the focusNode is Earth.*/
    RenderableCommunicationPackage::LineVBOLayout 
        CommunicationLines::getPositionForGeocentricSceneGraphNode(const char* id) {

        glm::vec3 nodePos = global::renderEngine.scene()->sceneGraphNode(id)->position();
        RenderableCommunicationPackage::LineVBOLayout position;
        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;

        return position;
    }


} // namespace openspace


