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
        glGenVertexArrays(1, &_mainRenderInformation._vaoID);
        glGenBuffers(1, &_mainRenderInformation._vBufferID);
    }
    void CommunicationLines::deinitializeGL(){
        glDeleteVertexArrays(1, &_mainRenderInformation._vaoID);
        glDeleteBuffers(1, &_mainRenderInformation._vBufferID);


        RenderableCommunicationPackage::deinitializeGL();
    }
    void CommunicationLines::update(const UpdateData& data){

            //Update vertex array with values from data 
            //DsnManager::fillVertexArray(_vertexArray);

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

            //int numVertices = signalDataVector.signals.size() * 2;

            // Make space for the vertices
            _vertexArray.clear();
            //_vertexArray.resize(numVertices);

            for (int i = 0; i < signalDataVector.signals.size(); i++) {
               
                Signal currentSignal = signalDataVector.signals[i];

                _vertexArray.push_back(getPositionForGeocentricSceneGraphNode(currentSignal.station));
                _vertexArray.push_back(getHighPrecisionPositionForSceneGraphNode(currentSignal.spacecraft));
            }

     
            // ... and upload them to the GPU
            glBindVertexArray(_mainRenderInformation._vaoID);
            glBindBuffer(GL_ARRAY_BUFFER, _mainRenderInformation._vBufferID);
            glBufferData(
                GL_ARRAY_BUFFER,
                _vertexArray.size() * sizeof(PackageVBOLayout),
                _vertexArray.data(),
                GL_STATIC_DRAW
            );

            glEnableVertexAttribArray(0);
            glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

            // We clear the indexArray just in case. The base class will take care not to use
            // it if it is empty
            //_indexArray.clear();

        // If the full trail should be rendered at all times, we can directly render the
        // entire set
        _mainRenderInformation.first = 0;
        _mainRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
     
        glBindVertexArray(0);
    }


    //Our spacecrafts are often very far from Earth (and the sun) which gives precision errors when getting close
    //To correct this we change the reference coordinate system when rendering these points if we want to zoom in on them

    RenderableCommunicationPackage::PackageVBOLayout CommunicationLines::getHighPrecisionPositionForSceneGraphNode( std::string id) {
        
        SceneGraphNode* focusNode = global::navigationHandler.focusNode();
        RenderableCommunicationPackage::PackageVBOLayout position;
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
                //_mainRenderInformation._localTransformSpacecraft;

                    glm::dvec3 earthPos(global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition());
                    // And get the current location of the object
                    const glm::dvec3 spacecraftPosWorld = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();
                    const glm::dvec3 v1 = { spacecraftPosWorld.x, spacecraftPosWorld.y, spacecraftPosWorld.z };

                //    // Compute the difference between the points in double precision
                    //const glm::dvec3 p0 = earthPos - v1; //earthpos - spacecraftposworld
                    //nodePos = {
                    //    static_cast<float>(p0.x),
                    //    static_cast<float>(p0.y),
                    //    static_cast<float>(p0.z)
                    //};
                    nodePos = {0.0,0.0,0.0};
                    //_auxiliaryVBOdata[1] = { 0.f, 0.f, 0.f };

                //    // Fill the render info with the data
                //    _mainRenderInformation.first = 0;
                //    _mainRenderInformation.count = 2;

                    _mainRenderInformation._localTransformSpacecraft = glm::translate(glm::dmat4(1.0), v1);
                //nodePos = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();


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

    RenderableCommunicationPackage::PackageVBOLayout CommunicationLines::getPositionForSceneGraphNode(const char* id) {

        glm::vec3 nodePos = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();
        RenderableCommunicationPackage::PackageVBOLayout position;
        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;

        return position;
    }

    //Since our station dishes have a static translation from earth, we can get their local translation
    RenderableCommunicationPackage::PackageVBOLayout CommunicationLines::getPositionForGeocentricSceneGraphNode(const char* id) {

        glm::vec3 nodePos = global::renderEngine.scene()->sceneGraphNode(id)->position();
        RenderableCommunicationPackage::PackageVBOLayout position;
        position.x = nodePos.x;
        position.y = nodePos.y;
        position.z = nodePos.z;

        return position;
    }


    //return vertexarray to commmunicationline
    void CommunicationLines::fillVertexArray(std::vector<RenderableCommunicationPackage::PackageVBOLayout> &vertexArray) {

        //get number of lines to be drawn = count signals from data at this time
        const int nValues = 2;

        // ... fill all of the values, dummy values for now, should load from  _translation->position()
        const char* dishIdentifier = "DSS54";
        const char* spacecraftIdentifier = "Voyager_1";

        SceneGraphNode* dishNode = global::renderEngine.scene()->sceneGraphNode(dishIdentifier);
        SceneGraphNode* spaceCraftNode = global::renderEngine.scene()->sceneGraphNode(spacecraftIdentifier);
        SceneGraphNode* earthNode = global::renderEngine.scene()->sceneGraphNode("Earth");

        //glm::vec3 dishPos = dishNode->worldPosition();

        glm::vec3 dishPos = dishNode->position();

        vertexArray[0] = { static_cast<float>(dishPos.x), static_cast<float>(dishPos.y), static_cast<float>(dishPos.z) };

        //If spacecraft excists in open space, use that position. 
        if (global::renderEngine.scene()->sceneGraphNode(spacecraftIdentifier)) {
            //glm::mat4  earthTrans = earthNode->modelTransform();
            // glm::mat4  spaceCraftTrans = spaceCraftNode->modelTransform();
            //glm::mat4  spaceCraftTransInWorld = earthTrans * spaceCraftTrans;

            //glm::vec3 earthWorldPos = earthNode->worldPosition();
            //glm::vec3 earthPos = earthNode->position();

            glm::vec3 spaceCraftPos = spaceCraftNode->position();
            // glm::vec3 spaceCraftPos = spaceCraftNode->worldPosition();

            vertexArray[1] = { static_cast<float>(spaceCraftPos.x), static_cast<float>(spaceCraftPos.y), static_cast<float>(spaceCraftPos.z) };
        }
        else
        {
            //Else estimate the position of the spacecraft from Azimuth and elevation angles. 
            LDEBUG("No position data for the space craft, estimate position");
            glm::vec3 spaceCraftPos = DsnManager::approximateSpacecraftPosition(dishIdentifier, dishPos); // VGR2
            vertexArray[1] = { static_cast<float>(spaceCraftPos.x), static_cast<float>(spaceCraftPos.y), static_cast<float>(spaceCraftPos.z) };
        }

    }

    glm::vec3 CommunicationLines::approximateSpacecraftPosition(const char* dishId, glm::vec3 dishPos) {

        double dishPosXYZ[] = { dishPos.x, dishPos.y, dishPos.z };
        double dishPosLLA[] = { -35.383, 148.966, 692 };

        double azimuthAngle = 205.87; // angle from true north 
        double elevationAngle = 13.40; // angle from horizontal plane towards zenith
        double range = 1.7762947155343E10 * 1000; //DSS35 to VGR2

                                                  // spacecraft coordinates in RAE
        double spacecraftPosRAE[3] = { range, azimuthAngle, elevationAngle };
        double spacecraftPosXYZ[3] = {};

        // fill up spacecraftPosXYZ
        convertRaeToEcef(dishPosLLA, dishPosXYZ, spacecraftPosRAE, spacecraftPosXYZ);
        glm::vec3 position = { spacecraftPosXYZ[0],spacecraftPosXYZ[1],spacecraftPosXYZ[2] };

        //return position
        return position;
    }

    void CommunicationLines::convertRaeToEcef(double observerLla[], double observerXyz[],
        double objectRae[], double objectEcef[]) {

        double tempSez[] = { 0.0, 0.0, 0.0 };

        convertRaeToSez(observerLla, objectRae, tempSez);
        convertSezToEcef(observerLla, observerXyz, tempSez, objectEcef);
    }

    void CommunicationLines::convertRaeToSez(double observerLla[], double objectRae[], double objectSez[]) {
        double range, azimuth, elevation;
        range = objectRae[0];
        azimuth = objectRae[1];
        elevation = objectRae[2];

        // Compute needed math
        double slat = sin(deg2rad(observerLla[0]));
        double slon = sin(deg2rad(observerLla[1]));
        double clat = cos(deg2rad(observerLla[0]));
        double clon = cos(deg2rad(observerLla[1]));

        // Convert to radians
        azimuth = deg2rad(azimuth);
        elevation = deg2rad(elevation);

        // Convert
        objectSez[0] = -range * cos(elevation) * cos(azimuth);
        objectSez[1] = range * cos(elevation) * sin(azimuth);
        objectSez[2] = range * sin(elevation);
    }

    void CommunicationLines::convertSezToEcef(double observerLla[], double observerXyz[],
        double objectSez[], double objectEcef[]) {

        double south, east, zenith;
        south = objectSez[0];
        east = objectSez[1];
        zenith = objectSez[2];

        // Compute needed math
        double slat = sin(deg2rad(observerLla[0]));
        double slon = sin(deg2rad(observerLla[1]));
        double clat = cos(deg2rad(observerLla[0]));
        double clon = cos(deg2rad(observerLla[1]));

        // Convert
        objectEcef[0] = (slat * clon * south) + (-slon * east) + (clat * clon * zenith) + observerXyz[0];
        objectEcef[1] = (slat * slon * south) + (clon * east) + (clat * slon * zenith) + observerXyz[1];
        objectEcef[2] = (-clat * south) + (slat * zenith) + observerXyz[2];
    }


    double CommunicationLines::deg2rad(double degrees)
    {
        const double factor = glm::pi<double>() / 180;
        return degrees * factor;
    }

} // namespace openspace


