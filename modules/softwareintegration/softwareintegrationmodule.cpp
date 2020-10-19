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

#include <modules/softwareintegration/softwareintegrationmodule.h>

#include <modules/softwareintegration/rendering/renderablepointscloud.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

#include <iomanip>

using namespace std::string_literals;

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationModule";
} // namespace

namespace openspace {

    SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

    void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
        auto fRenderable = FactoryManager::ref().factory<Renderable>();
        ghoul_assert(fRenderable, "No renderable factory existed");

        fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

        // Open port
        start(4700);
    }

    void SoftwareIntegrationModule::internalDeinitialize() {
        stop();
    }

    void SoftwareIntegrationModule::internalDeinitializeGL() {

    }

    void SoftwareIntegrationModule::start(int port)
    {
        _socketServer.listen(port);

        _serverThread = std::thread([this]() { handleNewPeers(); });
        _eventLoopThread = std::thread([this]() { eventLoop(); });
    }

    void SoftwareIntegrationModule::stop() {
        _shouldStop = true;
        _socketServer.close();

        if (_serverThread.joinable()) {
            _serverThread.join();
        }
        if (_eventLoopThread.joinable()) {
            _eventLoopThread.join();
        }
    }

    bool SoftwareIntegrationModule::isConnected(const Peer& peer) const {
        return peer.status != SoftwareConnection::Status::Connecting &&
            peer.status != SoftwareConnection::Status::Disconnected;
    }

    void SoftwareIntegrationModule::disconnect(Peer& peer) {
        if (isConnected(peer)) {
            _nConnections = nConnections() - 1;
        }

        peer.connection.disconnect();
        peer.thread.join();
        _peers.erase(peer.id);
    }

    void SoftwareIntegrationModule::handleNewPeers() {
        while (!_shouldStop) {
            std::unique_ptr<ghoul::io::TcpSocket> socket =
                _socketServer.awaitPendingTcpSocket();

            if (socket != nullptr)
                socket->startStreams();
            else
                return;

            const size_t id = _nextConnectionId++;
            std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
                id,
                "",
                SoftwareConnection(std::move(socket)),
                SoftwareConnection::Status::Connecting,
                std::thread()
                });
            auto it = _peers.emplace(p->id, p);
            it.first->second->thread = std::thread([this, id]() {
                handlePeer(id);
            });
        }
    }

    void SoftwareIntegrationModule::eventLoop() {
        while (!_shouldStop) {
            if (!_incomingMessages.empty()) {
                PeerMessage pm = _incomingMessages.pop();
                handlePeerMessage(std::move(pm));
            }
        }
    }

    std::shared_ptr<SoftwareIntegrationModule::Peer> SoftwareIntegrationModule::peer(size_t id) {
        std::lock_guard<std::mutex> lock(_peerListMutex);
        auto it = _peers.find(id);
        if (it == _peers.end()) {
            return nullptr;
        }
        return it->second;
    }

    void SoftwareIntegrationModule::handlePeer(size_t id) {
        while (!_shouldStop) {
            std::shared_ptr<Peer> p = peer(id);
            if (!p) {
                return;
            }

            if (!p->connection.isConnectedOrConnecting()) {
                return;
            }
            try {
                SoftwareConnection::Message m = p->connection.receiveMessage();
                _incomingMessages.push({ id, m });
            }
            catch (const SoftwareConnection::SoftwareConnectionLostError&) {
                LERROR(fmt::format("Connection lost to {}", p->id));
                _incomingMessages.push({
                    id,
                    SoftwareConnection::Message(
                        SoftwareConnection::MessageType::Disconnection, std::vector<char>()
                    )
                    });
                return;
            }
        }
    }

    void SoftwareIntegrationModule::handlePeerMessage(PeerMessage peerMessage) {
        const size_t peerId = peerMessage.peerId;
        auto it = _peers.find(peerId);
        if (it == _peers.end()) {
            return;
        }
        std::shared_ptr<Peer>& peer = it->second;
        
        const SoftwareConnection::MessageType messageType = peerMessage.message.type;
        std::vector<char>& message = peerMessage.message.content;
        switch (messageType) {
        case SoftwareConnection::MessageType::Connection: {
            std::string software(message.begin(), message.end());
            LINFO(fmt::format("OpenSpace has connected with {} through socket.", software));
            break;
        } 
        case SoftwareConnection::MessageType::ReadPointData: {
            messageOffset = 0; // Resets message offset 

            std::vector<float> xCoordinates = readData(message);
            std::vector<float> yCoordinates = readData(message);
            std::vector<float> zCoordinates = readData(message);

            int size = xCoordinates.size();
            pointData.clear();

            for (int i = 0; i < size; i++) {
                float x = xCoordinates[i];
                float y = yCoordinates[i];
                float z = zCoordinates[i];

                pointData.push_back({ x, y, z });
            }
            break;
        }
        case SoftwareConnection::MessageType::ReadLuminosityData: {
            messageOffset = 0; // Resets message offset 

            luminosityData.clear();
            luminosityData = readData(message);
            break;
        }
        case SoftwareConnection::MessageType::ReadVelocityData: {
            messageOffset = 0; // Resets message offset 

            velocityData.clear();
            velocityData = readData(message);
            break;
        }
        case SoftwareConnection::MessageType::AddSceneGraphNode: {
            // The following order of creating variables is the exact order they're received in the message
            // If the order is not the same, the global variable 'message offset' will be wrong
            std::string identifier = readIdentifier(message);
            glm::vec3 color = readColor(message);
            float opacity = readFloatValue(message);
            float size = readFloatValue(message);
            std::string guiName = readGUI(message);
            
            bool hasLuminosityData = !luminosityData.empty();
            bool hasVelocityData = !velocityData.empty();
            ghoul::Dictionary renderable;
            ghoul::Dictionary pointDataDictonary;
            ghoul::Dictionary luminosityDataDictonary;
            ghoul::Dictionary velocityDataDictionary;
            for (int i = 0; i < pointData.size(); ++i) {
                pointDataDictonary.setValue<glm::vec3>(std::to_string(i + 1), pointData[i]);
            }
            if (hasLuminosityData) {
                for (int i = 0; i < luminosityData.size(); ++i) {
                    luminosityDataDictonary.setValue<float>(std::to_string(i + 1), luminosityData[i]);
                }
            }
            if (hasVelocityData) {
                for (int i = 0; i < velocityData.size(); ++i) {
                    velocityDataDictionary.setValue<float>(std::to_string(i + 1), velocityData[i]);
                }
            }

            // Create a renderable depending on what data was received
            if (hasLuminosityData && hasVelocityData) {
                renderable = {
                    { "Type", "RenderablePointsCloud"s },
                    { "Color", static_cast<glm::dvec3>(color)},
                    { "Data", pointDataDictonary },
                    { "Luminosity", luminosityDataDictonary },
                    { "Opacity", static_cast<double>(opacity) },
                    { "Size", static_cast<double>(size)},
                    { "Velocity", velocityDataDictionary }
                };
            }
            else if (hasLuminosityData && !hasVelocityData) {
                renderable = {
                    { "Type", "RenderablePointsCloud"s },
                    { "Color", static_cast<glm::dvec3>(color)},
                    { "Data", pointDataDictonary },
                    { "Luminosity", luminosityDataDictonary },
                    { "Opacity", static_cast<double>(opacity) },
                    { "Size", static_cast<double>(size)},
                };
            }
            else if (!hasLuminosityData && hasVelocityData) {
                renderable = {
                    { "Type", "RenderablePointsCloud"s },
                    { "Color", static_cast<glm::dvec3>(color)},
                    { "Data", pointDataDictonary },
                    { "Opacity", static_cast<double>(opacity) },
                    { "Size", static_cast<double>(size)},
                    { "Velocity", velocityDataDictionary }
                };
            }
            else {
                renderable = {
                    { "Type", "RenderablePointsCloud"s },
                    { "Color", static_cast<glm::dvec3>(color)},
                    { "Data", pointDataDictonary },
                    { "Opacity", static_cast<double>(opacity) },
                    { "Size", static_cast<double>(size)},
                };
            }
            
            ghoul::Dictionary gui = {
                { "Name", guiName },
                { "Path", "/Examples"s }
            };

            ghoul::Dictionary node = {
                { "Identifier", identifier },
                { "Renderable", renderable },
                { "GUI", gui }
            };

            try {
                SceneGraphNode* sgn = global::renderEngine.scene()->loadNode(node);
                if (!sgn) {
                    LERROR("Scene", "Could not load scene graph node");
                    break;
                }
                global::renderEngine.scene()->initializeNode(sgn);

                openspace::global::scriptEngine.queueScript(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + identifier + "')"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
            catch (const documentation::SpecificationError& e) {
                return LERROR(fmt::format("Documentation SpecificationError: Error loading scene graph node {}",
                        e.what())
                );
            }
            catch (const ghoul::RuntimeError& e) {
                return LERROR(fmt::format("RuntimeError: Error loading scene graph node {}",
                    e.what())
                );
            }

            handleProperties(identifier, peer);
            break;
        }
        case SoftwareConnection::MessageType::RemoveSceneGraphNode: {
            std::string identifier(message.begin(), message.end());

            SceneGraphNode* sgn = global::renderEngine.scene()->sceneGraphNode(identifier);
            
            if (global::navigationHandler.orbitalNavigator().anchorNode() == sgn) {
                openspace::global::scriptEngine.queueScript(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Sun')"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
            openspace::global::scriptEngine.queueScript(
                "openspace.removeSceneGraphNode('" + identifier + "');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            LINFO(fmt::format("Scengraph {} removed.", identifier));
            break;
        }
        case SoftwareConnection::MessageType::Color: {
            std::string msg(message.begin(), message.end());
            LINFO(fmt::format("Message recieved: {}", msg));
            std::string identifier = readIdentifier(message);
            glm::vec3 color = readColor(message);

            // Get color of renderable
            const Renderable* myRenderable = renderable(identifier);
            properties::Property* colorProperty = myRenderable->property("Color");
            auto propertyAny = colorProperty->get();
            glm::vec3 propertyColor = std::any_cast<glm::vec3>(propertyAny);
            bool isUpdated = (propertyColor != color);

            // Update color of renderable
            if (isUpdated)
                colorProperty->set(color);
            break;
        }
        case SoftwareConnection::MessageType::Opacity: {
            std::string msg(message.begin(), message.end());
            LINFO(fmt::format("Message recieved: {}", msg));
            std::string identifier = readIdentifier(message);
            float opacity = readFloatValue(message);

            // Get opacity of renderable
            const Renderable* myRenderable = renderable(identifier);
            properties::Property* opacityProperty = myRenderable->property("Opacity");
            auto propertyAny = opacityProperty->get();
            float propertyOpacity = std::any_cast<float>(propertyAny);
            bool isUpdated = (propertyOpacity != opacity);

            // Update opacity of renderable
            if (isUpdated)
                opacityProperty->set(opacity);
            break;
        }
        case SoftwareConnection::MessageType::Size: {
            std::string msg(message.begin(), message.end());
            LINFO(fmt::format("Message recieved: {}", msg));
            std::string identifier = readIdentifier(message);
            float size = readFloatValue(message);

            // Get size of renderable
            const Renderable* myRenderable = renderable(identifier);
            properties::Property* sizeProperty = myRenderable->property("Size");
            auto propertyAny = sizeProperty->get();
            float propertySize = std::any_cast<float>(propertyAny);
            bool isUpdated = (propertySize != size);

            // Update size of renderable
            if (isUpdated)
                sizeProperty->set(size);
            break;
        }
        case SoftwareConnection::MessageType::Visibility: {
            std::string msg(message.begin(), message.end());
            LINFO(fmt::format("Message recieved: {}", msg));
            std::string identifier = readIdentifier(message);
            std::string visibility;
            visibility.push_back(message[messageOffset]);

            // Toggle visibility of renderable
            const Renderable* myRenderable = renderable(identifier);
            properties::Property* visibilityProperty = myRenderable->property("ToggleVisibility");
            if(visibility == "F")
                visibilityProperty->set(false);
            else
                visibilityProperty->set(true);
            break;
        }
        case SoftwareConnection::MessageType::Disconnection: {
            disconnect(*peer);
            break;
        }
        default:
            LERROR(fmt::format(
                "Unsupported message type: {}", static_cast<int>(messageType)
            ));
            break;
        }
    }

    void SoftwareIntegrationModule::handleProperties(std::string identifier, const std::shared_ptr<Peer>& peer) {
        const Renderable* myRenderable = renderable(identifier);
        properties::Property* colorProperty = myRenderable->property("Color");
        properties::Property* opacityProperty = myRenderable->property("Opacity");
        properties::Property* sizeProperty = myRenderable->property("Size");
        properties::Property* visibilityProperty = myRenderable->property("ToggleVisibility");

        // Update color of renderable
        auto updateColor = [colorProperty, identifier, peer]() {
            std::string lengthOfIdentifier = std::to_string(identifier.length());
            std::string propertyValue = colorProperty->getStringValue();
            std::string lengthOfValue = std::to_string(propertyValue.length());
            std::string messageType = "UPCO";
            std::string subject = lengthOfIdentifier + identifier + lengthOfValue + propertyValue;

            // Format length of subject to always be 4 digits
            std::ostringstream os;
            os << std::setfill('0') << std::setw(4) << subject.length();
            std::string lengthOfSubject = os.str();

            std::string message = messageType + lengthOfSubject + subject;
            peer->connection.sendMessage(message);
        };
        colorProperty->onChange(updateColor);

        // Update opacity of renderable
        auto updateOpacity = [opacityProperty, identifier, peer]() {
            std::string lengthOfIdentifier = std::to_string(identifier.length());
            std::string propertyValue = opacityProperty->getStringValue();
            std::string lengthOfValue = std::to_string(propertyValue.length());
            std::string messageType = "UPOP";
            std::string subject = lengthOfIdentifier + identifier + lengthOfValue + propertyValue;

            // Format length of subject to always be 4 digits
            std::ostringstream os;
            os << std::setfill('0') << std::setw(4) << subject.length();
            std::string lengthOfSubject = os.str();

            std::string message = messageType + lengthOfSubject + subject;
            peer->connection.sendMessage(message);
        };
        opacityProperty->onChange(updateOpacity);

        // Update size of renderable
        auto updateSize = [sizeProperty, identifier, peer]() {
            std::string lengthOfIdentifier = std::to_string(identifier.length());
            std::string propertyValue = sizeProperty->getStringValue();
            std::string lengthOfValue = std::to_string(propertyValue.length());
            std::string messageType = "UPSI";
            std::string subject = lengthOfIdentifier + identifier + lengthOfValue + propertyValue;

            // Format length of subject to always be 4 digits
            std::ostringstream os;
            os << std::setfill('0') << std::setw(4) << subject.length();
            std::string lengthOfSubject = os.str();

            std::string message = messageType + lengthOfSubject + subject;
            peer->connection.sendMessage(message);
        };
        sizeProperty->onChange(updateSize);

        // Toggle visibility of renderable
        auto toggleVisibility = [visibilityProperty, identifier, peer]() {
            std::string lengthOfIdentifier = std::to_string(identifier.length());
            std::string messageType = "TOVI";

            std::string propertyValue;
            if (visibilityProperty->getStringValue() == "false")
                propertyValue = "F";
            else
                propertyValue = "T";

            std::string subject = lengthOfIdentifier + identifier + propertyValue;
            // We don't need a lengthOfValue here because it will always be 1 character 

            // Format length of subject to always be 4 digits
            std::ostringstream os;
            os << std::setfill('0') << std::setw(4) << subject.length();
            std::string lengthOfSubject = os.str();

            std::string message = messageType + lengthOfSubject + subject;
            peer->connection.sendMessage(message);
        };
        visibilityProperty->onChange(toggleVisibility);
    }

    // Read size value or opacity value
    float SoftwareIntegrationModule::readFloatValue(std::vector<char>& message) {
        std::string length;
        length.push_back(message[messageOffset]);
        messageOffset++;

        int lengthOfValue = stoi(length);
        std::string value;
        int counter = 0;
        while (counter != lengthOfValue)
        {
            value.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }
        float floatValue = std::stof(value);

        return floatValue;
    }

    glm::vec3 SoftwareIntegrationModule::readColor(std::vector<char>& message) {
        std::string lengthOfColor; // Not used for now, but sent in message
        lengthOfColor.push_back(message[messageOffset]);
        messageOffset++;
        lengthOfColor.push_back(message[messageOffset]);
        messageOffset++;

        // Color is recieved in a string-format of (redValue, greenValue, blueValue)
        // Therefore, we have to iterate through the message and ignore characters
        // "( , )" and separate the values in the string
        std::string red;
        while (message[messageOffset] != ',')
        {
            if (message[messageOffset] == '(')
                messageOffset++;
            else {
                red.push_back(message[messageOffset]);
                messageOffset++;
            }
        }
        messageOffset++;

        std::string green;
        while (message[messageOffset] != ',')
        {
            green.push_back(message[messageOffset]);
            messageOffset++;
        }
        messageOffset++;

        std::string blue;
        while (message[messageOffset] != ')')
        {
            blue.push_back(message[messageOffset]);
            messageOffset++;
        }
        messageOffset++;

        // Convert red, green, blue strings to floats
        float r = std::stof(red);
        float g = std::stof(green);
        float b = std::stof(blue);
        glm::vec3 color(r, g, b);

        return color;
    }

    std::vector<float> SoftwareIntegrationModule::readData(std::vector<char>& message) {
        std::string length;
        int lengthOffset = messageOffset + 9; // 9 first bytes is the length of the data

        for (int i = messageOffset; i < lengthOffset; i++)
        {
            length.push_back(message[i]);
            messageOffset++;
        }
           
        int lengthOfData = stoi(length);
        int counter = 0;

        std::vector< float > data;
        std::string value;

        while (counter != lengthOfData)
        {
            while (message[messageOffset] != ',')
            {
                value.push_back(message[messageOffset]);
                messageOffset++;
                counter++;
            }
            float dataValue = stof(value);
            data.push_back(dataValue);
            value = "";
            messageOffset++;
            counter++;
        }

        return data;
    }

    std::string SoftwareIntegrationModule::readIdentifier(std::vector<char>& message) {
        std::string length;
        length.push_back(message[0]);
        length.push_back(message[1]);

        int lengthOfIdentifier = stoi(length);
        int counter = 0;
        messageOffset = 2; // Resets messageOffset

        std::string identifier;
        while (counter != lengthOfIdentifier)
        {
            identifier.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }

        return identifier;
    }

    std::string SoftwareIntegrationModule::readGUI(std::vector<char>& message) {
        std::string length;
        length.push_back(message[messageOffset]);
        messageOffset++;
        length.push_back(message[messageOffset]);
        messageOffset++;

        int lengthOfGUI = stoi(length);
        std::string GUI;
        int counter = 0;
        while (counter != lengthOfGUI)
        {
            GUI.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }

        return GUI;
    }

    size_t SoftwareIntegrationModule::nConnections() const {
        return _nConnections;
    }

    std::vector<documentation::Documentation> SoftwareIntegrationModule::documentations() const {
        return {
            RenderablePointsCloud::Documentation(),
        };
    }

} // namespace openspace

