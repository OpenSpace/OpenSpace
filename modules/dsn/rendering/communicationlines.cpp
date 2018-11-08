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
        :RenderableSignals(dictionary){
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
        RenderableSignals::initializeGL();

        // We don't need an index buffer, so we keep it at the default value of 0
        glGenVertexArrays(1, &_lineRenderInformation._vaoID);
        glGenBuffers(1, &_lineRenderInformation._vBufferID);
    }
    void CommunicationLines::deinitializeGL(){
        glDeleteVertexArrays(1, &_lineRenderInformation._vaoID);
        glDeleteBuffers(1, &_lineRenderInformation._vBufferID);

        RenderableSignals::deinitializeGL();
    }
    
    glm::vec3 CommunicationLines::convertRaDecRangeToCartesian() {

       //Dummy data for voyager 1
       float ra = 257.777029167736; //2018-246
       float dec = 12.2537708651048; // 2018-246
       float range = 2.14044781771236e+13; 
       
        // Convert RA and DEC from degrees to radians 
        ra = glm::radians(ra);
        dec = glm::radians(dec);

        //Save the cartesian coordinates
        double cartesianCoordinates[3];

        //Fill array with coordinates 
        radrec_c(range, ra, dec, cartesianCoordinates);

        //Save array in vector 
        glm::vec3 raDecPos = { cartesianCoordinates[0],cartesianCoordinates[1], cartesianCoordinates[2] };
        
        //Get the RA / DEC values in world coordinates with respect to the current focus node
        raDecPos = getEstimatedCoordinatePosFromFocusNode(raDecPos);

        return raDecPos;
    }


    void CommunicationLines::update(const UpdateData& data) {


        double currentTime = data.time.j2000Seconds();
        //Todo: change this magic number. 86400 equals 24hrs in seconds
        double endTime = 86400; 

        //Bool if the current time is within the timeframe for the currently loaded data
        const bool isTimeInFileInterval = (currentTime >= DsnManager::_dsnData.sequenceStartTime) &&
            (currentTime < DsnManager::_dsnData.sequenceStartTime + endTime);

        //Reload data if it is not relevant anymore
        if (!isTimeInFileInterval) {
            DsnManager::_dsnData.isLoaded = false;

            int activeFileIndex = findFileIndexForCurrentTime(currentTime, DsnManager::_fileStartTimes);
            //parse data for that file
            if (!DsnManager::_dsnData.isLoaded)
            {
                DsnManager::jsonParser(activeFileIndex);

            }
            else
                return;
        }

        // Make space for the vertex renderinformation
        _vertexArray.clear();

        //update focusnode information used to calculate spacecraft positions
        _focusNode = global::navigationHandler.focusNode();
        _lineRenderInformation._localTransformSpacecraft = glm::translate(glm::dmat4(1.0), _focusNode->worldPosition());

        //Todo; keep track of active index for signalvector, or swap for loop for binary search
        for (int i = 0; i < DsnManager::_dsnData.signals.size(); i++) {
               
            DsnManager::Signal currentSignal = DsnManager::_dsnData.signals[i];
            if(isSignalActive(currentTime, currentSignal.startTime,currentSignal.endTime))
                pushSignalDataToVertexArray(currentSignal);
        };


        // ... and upload them to the GPU
        glBindVertexArray(_lineRenderInformation._vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _lineRenderInformation._vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexArray.size() * sizeof(float),
            _vertexArray.data(),
            GL_STATIC_DRAW
        );

        // position attributes
        glVertexAttribPointer(_locVer, _sizePosVal, GL_FLOAT, GL_FALSE, sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout), (void*)0);
        glEnableVertexAttribArray(_locVer);
        // color attributes
        glVertexAttribPointer(_locCol, _sizeColorVal, GL_FLOAT, GL_FALSE, sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout), (void*)(sizeof(PositionVBOLayout)));
        glEnableVertexAttribArray(_locCol);

        // Directly render the entire step
        _lineRenderInformation.first = 0;
        _lineRenderInformation.count = static_cast<GLsizei>(_vertexArray.size()/(_sizePosVal + _sizeColorVal));
        
        //unbind vertexArray
        glBindVertexArray(0);
    }

    int CommunicationLines::findFileIndexForCurrentTime(double time, std::vector<double> vec) {
        // upper_bound has O(log n) for sorted vectors, more efficient than for loop
        auto iter = std::upper_bound(vec.begin(), vec.end(), time);

        int fileIndex = -1;
        //check what index we got 
        if (iter != vec.end()) {
            if (iter != vec.begin()) {
                fileIndex = static_cast<int>(
                    std::distance(vec.begin(), iter)
                    ) - 1;
            }
            else {
                fileIndex = 0;
            }
        }
        else {
            fileIndex = static_cast<int>(vec.size()) - 1;
        }

        return fileIndex;
    }


    void CommunicationLines::pushRaDecDataToVertexArray(glm::vec3 raDecPos) {

        ColorVBOLayout color0 = getSiteColor("DSS63"); // red
        ColorVBOLayout color1 = getSiteColor("DSS43"); //green
        ColorVBOLayout color2 = getSiteColor("DSS14"); // blue

        glm::vec3 posEarth = { 0.0, 0.0, 0.0 };

        _vertexArray.push_back(raDecPos.x);
        _vertexArray.push_back(raDecPos.y);
        _vertexArray.push_back(raDecPos.z);

        _vertexArray.push_back(color0.r);
        _vertexArray.push_back(color0.g);
        _vertexArray.push_back(color0.b);
        _vertexArray.push_back(color0.a);

        _vertexArray.push_back(posEarth.x);
        _vertexArray.push_back(posEarth.y);
        _vertexArray.push_back(posEarth.z);

        _vertexArray.push_back(color1.r);
        _vertexArray.push_back(color1.g);
        _vertexArray.push_back(color1.b);

    }

    void CommunicationLines::pushSignalDataToVertexArray(DsnManager::Signal signal) {

        ColorVBOLayout color = getSiteColor(signal.dishName);
        PositionVBOLayout posStation = getPositionForGeocentricSceneGraphNode(signal.dishName.c_str());
        PositionVBOLayout posSpacecraft = getSuitablePrecisionPositionForSceneGraphNode(signal.spacecraft.c_str());

        //fill the render array
        _vertexArray.push_back(posStation.x);
        _vertexArray.push_back(posStation.y);
        _vertexArray.push_back(posStation.z);

        _vertexArray.push_back(color.r);
        _vertexArray.push_back(color.g);
        _vertexArray.push_back(color.b);
        _vertexArray.push_back(color.a);

        _vertexArray.push_back(posSpacecraft.x);
        _vertexArray.push_back(posSpacecraft.y);
        _vertexArray.push_back(posSpacecraft.z);

        _vertexArray.push_back(color.r);
        _vertexArray.push_back(color.g);
        _vertexArray.push_back(color.b);
        _vertexArray.push_back(color.a);

    }

    bool CommunicationLines::isSignalActive(double currentTime, std::string signalStartTime, std::string signalEndTime) {
        double startTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalStartTime);
        double endTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalEndTime);

        if (startTimeInSeconds <= currentTime && endTimeInSeconds >= currentTime)
            return true;

        return false;
    }

    /* Since our station dishes have a static translation from Earth, we
    * can get their local translation. The reason to handle it differently
    * compared to the spacecrafts is to keep an exact render position
    * for the station line ends even when the focusNode is Earth. */
    glm::dvec3 CommunicationLines::getCoordinatePosFromFocusNode(SceneGraphNode* node) {

        glm::dvec3 nodePos = node->worldPosition();
        glm::dvec3 focusNodePos = _focusNode->worldPosition();

        glm::dvec3 diff = glm::vec3(nodePos.x - focusNodePos.x, nodePos.y - focusNodePos.y,
            nodePos.z - focusNodePos.z);

        return diff;
    }

    glm::dvec3 CommunicationLines::getEstimatedCoordinatePosFromFocusNode(glm::vec3 pos) {
        
        glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
        glm::dvec3 focusNodePos = _focusNode->worldPosition();

        glm::dmat4 translationMatrixEarth = glm::translate(glm::dmat4(1.0), glm::dvec3(earthPos));
       
        glm::dvec4 newPos = { pos, 1.0 };
        glm::dvec4 nodePos = _rotEquatorialSphere * translationMatrixEarth  *newPos;

        glm::dvec3 diff = glm::vec3(nodePos.x - focusNodePos.x, nodePos.y - focusNodePos.y,
            nodePos.z - focusNodePos.z);


        return diff;
    }

    /* Our spacecrafts are often very far from Earth (and the sun) which gives precision
    * errors when we getting close to the node. To correct this we make a localtransform
    * to the current focusNode and calculate positions with the focusNode as origin. This
    * gives us a exact renderposition when we get closer to spacecrafts */
    RenderableSignals::PositionVBOLayout
        CommunicationLines::getSuitablePrecisionPositionForSceneGraphNode(std::string id) {
        
        RenderableSignals::PositionVBOLayout position;
        glm::dvec3 nodePos;

        if (global::renderEngine.scene()->sceneGraphNode(id)) {
            SceneGraphNode* spacecraftNode = global::renderEngine.scene()->sceneGraphNode(id);
            nodePos = getCoordinatePosFromFocusNode(spacecraftNode);
        }
        else {
            // TODO: Else estimate the position of the spacecraft by RA/DEC/RANGE 
           // LDEBUG(fmt::format("No position data for the spacecraft {}, estimating position",id));
            nodePos = convertRaDecRangeToCartesian();
        }

        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;
       
        return position;
    }

    RenderableSignals::PositionVBOLayout
        CommunicationLines::getPositionForGeocentricSceneGraphNode(const char* id) {
        
        RenderableSignals::PositionVBOLayout position;
        glm::dvec3 nodePos;

        if (global::renderEngine.scene()->sceneGraphNode(id)) {
            nodePos = global::renderEngine.scene()->sceneGraphNode(id)->position();
        }
        else {
            LERROR(fmt::format("No position data for the station dish {}, drawing line from center of Earth", id));
            nodePos = glm::vec3(0, 0, 0);
        }

        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;

        return position;
    }


} // namespace openspace


