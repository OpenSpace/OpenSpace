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
#include <openspace/scene/scene.h>
#include <ghoul/glm.h>


 //Debug purposes
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <thread>
#include <chrono>

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

//Send message to supercollider, address the message with label
//NOTE: label must be in the format: "/label"
void SonificationModule::sendMesssage(const std::string label, const float message) {
    //NOTE: Socket cannot be saved in class, it does not work then, dont know why. 
    //Only works if the socket is recreated every time
    UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
    _stream.Clear();
    _stream << osc::BeginMessage(label.c_str()) << message << osc::EndMessage;
    socket.Send(_stream.Data(), _stream.Size());
}

//Send message to supercollider, address the message with label
    //NOTE: label must be in the format: "/label"
void SonificationModule::sendMesssage(const std::string label, const std::string message) {
    //NOTE: Socket cannot be saved in class, it does not work then, dont know why. 
    //Only works if the socket is recreated every time
    UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
    _stream.Clear();
    _stream << osc::BeginMessage(label.c_str()) << message.c_str() << osc::EndMessage;
    socket.Send(_stream.Data(), _stream.Size());
}

//Send message to supercollider, address the message with label
    //NOTE: label must be in the format: "/label"
void SonificationModule::sendMesssage(const std::string label, const glm::dvec3 message) {
    //NOTE: Socket cannot be saved in class, it does not work then, dont know why. 
    //Only works if the socket is recreated every time
    UdpTransmitSocket socket = UdpTransmitSocket(IpEndpointName(SC_IP_ADDRESS, SC_PORT));
    _stream.Clear();
    _stream << osc::BeginMessage(label.c_str()) << message.x << message.y << message.z << osc::EndMessage;
    socket.Send(_stream.Data(), _stream.Size());
}

void SonificationModule::threadFunk(std::atomic<bool>& isRunning) {
    
    //std::this_thread::sleep_for(std::chrono::seconds(10));
    
    while (isRunning) {
        /*float random = 10.0f + static_cast <float> (rand()) /
            (static_cast <float> (RAND_MAX / (100.0f - 10.0f)));
        std::cout << "Thread generated: " << random << std::endl;
        sendMesssage("/venus", random);*/

        //Is scene initialized?
        if (global::renderEngine.scene() && !global::renderEngine.scene()->isInitializing()) {
            //Mercury
            if (global::renderEngine.scene()->sceneGraphNode("Mercury")) {
                glm::dvec3 mercuryPos = global::renderEngine.scene()->sceneGraphNode("Mercury")->worldPosition();
                if (mercuryPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Mercury: ( " << mercuryPos.x << ", " << mercuryPos.y << ", " << mercuryPos.z << ")" << std::endl;
                    sendMesssage("/mercury", mercuryPos);
                } 
            }

            //Venus
            if (global::renderEngine.scene()->sceneGraphNode("Venus")) {
                glm::dvec3 venusPos = global::renderEngine.scene()->sceneGraphNode("Venus")->worldPosition();
                if (venusPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Venus: ( " << venusPos.x << ", " << venusPos.y << ", " << venusPos.z << ")" << std::endl;
                    sendMesssage("/venus", venusPos);
                }
            }

            //Earth
            if (global::renderEngine.scene()->sceneGraphNode("Earth")) {
                glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
                if (earthPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Earth: ( " << earthPos.x << ", " << earthPos.y << ", " << earthPos.z << ")" << std::endl;
                    sendMesssage("/earth", earthPos);
                }
            }

            //Mars
            if (global::renderEngine.scene()->sceneGraphNode("Mars")) {
                glm::dvec3 marsPos = global::renderEngine.scene()->sceneGraphNode("Mars")->worldPosition();
                if (marsPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Mars: ( " << marsPos.x << ", " << marsPos.y << ", " << marsPos.z << ")" << std::endl;
                    sendMesssage("/mars", marsPos);
                }
            }

            //Jupiter
            if (global::renderEngine.scene()->sceneGraphNode("Jupiter")) {
                glm::dvec3 jupiterPos = global::renderEngine.scene()->sceneGraphNode("Jupiter")->worldPosition();
                if (jupiterPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Jupiter: ( " << jupiterPos.x << ", " << jupiterPos.y << ", " << jupiterPos.z << ")" << std::endl;
                    sendMesssage("/jupiter", jupiterPos);
                }
            }

            //Saturn
            if (global::renderEngine.scene()->sceneGraphNode("Saturn")) {
                glm::dvec3 saturnPos = global::renderEngine.scene()->sceneGraphNode("Saturn")->worldPosition();
                if (saturnPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Saturn: ( " << saturnPos.x << ", " << saturnPos.y << ", " << saturnPos.z << ")" << std::endl;
                    sendMesssage("/saturn", saturnPos);
                }  
            }

            //Uranus
            if (global::renderEngine.scene()->sceneGraphNode("Uranus")) {
                glm::dvec3 uranusPos = global::renderEngine.scene()->sceneGraphNode("Uranus")->worldPosition();
                if (uranusPos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Uranus: ( " << uranusPos.x << ", " << uranusPos.y << ", " << uranusPos.z << ")" << std::endl;
                    sendMesssage("/uranus", uranusPos);
                }
            }

            //Neptune
            if (global::renderEngine.scene()->sceneGraphNode("Neptune")) {
                glm::dvec3 neptunePos = global::renderEngine.scene()->sceneGraphNode("Neptune")->worldPosition();
                if (neptunePos != glm::dvec3(0.0, 0.0, 0.0)) {
                    std::cout << "Thread: Position of Neptune: ( " << neptunePos.x << ", " << neptunePos.y << ", " << neptunePos.z << ")" << std::endl;
                    sendMesssage("/neptune", neptunePos);
                }
            } 
        }

        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
}

void SonificationModule::internalInitialize(const ghoul::Dictionary& /*dictionary*/) {
    //Test to send some data to SC
    //std::cout << "Sonification Initialize: Sending message to SuperCollider!" << std::endl;
    srand(static_cast <unsigned> (time(0)));
    sendMesssage("/venus", 1.0f);

    //start a thread
    _thread = std::thread([this]() { threadFunk(std::ref(_isRunning)); });

    //Wait a while and then close the thread
    /*std::this_thread::sleep_for(std::chrono::seconds(20));
    std::cout << "Joining thread!" << std::endl;
    if (th.joinable())
        th.join();*/

}



} // namespace openspace
