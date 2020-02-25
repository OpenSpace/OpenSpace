/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                              *
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

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/glm.h>
#include <openspace/util/camera.h>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/projection.hpp>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>


 //Debug purposes
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <thread>
#include <chrono>
#define _USE_MATH_DEFINES
#include <math.h>

//Output to SuperCollider (SC)
#define SC_IP_ADDRESS "127.0.0.1"
#define SC_PORT 57120
#define BUFFER_SIZE 1024

namespace openspace {


SonificationModule::SonificationModule()
    : OpenSpaceModule("Sonification")
{
    //Create socket and stream to send to SuperCollider
    _buffer = new char[BUFFER_SIZE];
    _stream = osc::OutboundPacketStream(_buffer, BUFFER_SIZE);
    _helper = SonificationHelper();
    _isRunning = true;

    if (_thread.joinable())
        _thread.join();
}

SonificationModule::~SonificationModule() {
    delete[] _buffer;
    _isRunning = false;
    if (_thread.joinable())
        _thread.join();
}

//Extract the data from the given identifier
//NOTE: The identifier must start with capital letter, otherwise no match will be found
void SonificationModule::extractData(const std::string& identifier,
    const Scene * const scene, const glm::dvec3& cameraPosition,
    const glm::dvec3& cameraDirection, const glm::dvec3& cameraUpVector)
{
    SceneGraphNode* node = scene->sceneGraphNode(identifier);
    if (node) {
        glm::dvec3 nodePosition = node->worldPosition();
        if (nodePosition != glm::dvec3(0.0, 0.0, 0.0)) {
            //std::cout << "Position of " << identifier << ": ( " << nodePosition.x << ", " << nodePosition.y << ", " << nodePosition.z << ")" << std::endl;
            
            //Calculate distance to the planet from the camera, convert to km
            glm::dvec3 cameraToNode = nodePosition - cameraPosition;
            double distance = glm::length(cameraToNode)/ 1000.0;
            //std::cout << "Distance from camera to " << identifier << ": " << distance << std::endl;

            //Calculate angle from camera to the planet in the camera plane
            //Project v down to the camera plane, Pplane(v)
            //Pn(v) is v projected on the normal n of the plane -> Pplane(v) = v - Pn(v)
            glm::dvec3 projectedNodePos = nodePosition - glm::proj(cameraToNode, cameraUpVector);
            double angle = glm::angle(glm::normalize(cameraDirection), glm::normalize(cameraToNode));
            
            //Convert to degrees, DEBUG
            double degreeAngle = angle * 180.0 / M_PI;
            //std::cout << "Angle between camera and " << identifier << ": " << degreeAngle << std::endl;
            
            //Send the data to SuperCollider
            std::string label = "/" + identifier;

            //NOTE: Socket cannot be saved in class, it does not work then, dont know why. 
            //Only works if the socket is recreated every time, wasteful
            UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
            _stream.Clear();
            _stream << osc::BeginMessage(label.c_str()) << distance << angle << osc::EndMessage;
            socket.Send(_stream.Data(), _stream.Size());
        }
    }
}

void SonificationModule::threadMain(std::atomic<bool>& isRunning) {
    
    Scene* scene;
    Camera* camera;
    glm::dvec3 cameraDir, cameraPos, cameraUpVector;
    const SceneGraphNode* focusNode;

    while (isRunning) {

        scene = global::renderEngine.scene();
        if (scene && !scene->isInitializing()) {
           
            camera = scene->camera();
            
            if (camera) {
                cameraPos = camera->positionVec3();
                cameraDir = camera->viewDirectionWorldSpace();
                cameraUpVector = camera->lookUpVectorWorldSpace();

                //Complete scene initialized, start extracting data
                if (cameraPos != glm::dvec3(1.0, 1.0, 1.0)) {
                    
                    //Which node is in focus?
                    focusNode = global::navigationHandler.orbitalNavigator().anchorNode();
                    if (!focusNode) continue;
                    std::cout << "Node in focus: " << focusNode->identifier() << std::endl;

                    //Let the sonification know which node is in focus,
                    //is used to switch different sonifications
                    UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
                    _stream.Clear();
                    std::string focusLabel = "/focus";
                    _stream << osc::BeginMessage(focusLabel.c_str()) << focusNode->identifier().c_str() << osc::EndMessage;
                    socket.Send(_stream.Data(), _stream.Size());

                    //If the sun is in focus then turn on the solrar system sonification
                    if (focusNode->identifier() == "Sun") {
                        //Mercury
                        extractData("Mercury", scene, cameraPos, cameraDir, cameraUpVector);

                        //Venus
                        extractData("Venus", scene, cameraPos, cameraDir, cameraUpVector);

                        //Earth
                        extractData("Earth", scene, cameraPos, cameraDir, cameraUpVector);

                        //Mars
                        extractData("Mars", scene, cameraPos, cameraDir, cameraUpVector);

                        //Jupiter
                        extractData("Jupiter", scene, cameraPos, cameraDir, cameraUpVector);

                        //Saturn
                        extractData("Saturn", scene, cameraPos, cameraDir, cameraUpVector);

                        //Uranus
                        extractData("Uranus", scene, cameraPos, cameraDir, cameraUpVector);

                        //Neptune
                        extractData("Neptune", scene, cameraPos, cameraDir, cameraUpVector);
                    }

                    //Otherwise just extract data from the node in focus, planetary sonification
                    else {
                        extractData(focusNode->identifier(), scene, cameraPos, cameraDir, cameraUpVector);
                    }
                }
            }
        }

        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
}

void SonificationModule::internalInitialize(const ghoul::Dictionary& /*dictionary*/) {
    //Test to send some data to SC
    //std::cout << "Sonification Initialize: Sending message to SuperCollider!" << std::endl;
    //sendMesssage("/venus", 1.0f);

    //start a thread
    _thread = std::thread([this]() { threadMain(std::ref(_isRunning)); });
}



} // namespace openspace
