/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
#define __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__

#include <openspace/util/openspacemodule.h>
#include <modules/server/connectionpool.h>
#include <ghoul/io/socket/tcpsocketserver.h>

#include <deque>
#include <memory>
#include <thread>
#include <mutex>
#include <cstdint>

namespace openspace {

enum class SocketAction : uint32_t {
    Open = 0,
    Data = 1,
    Close = 2
};

enum class ChannelAction : uint32_t {
    Initialize = 0,
    Data = 1,
    Deinitialize = 2
};

struct Message {
    std::shared_ptr<ghoul::io::Socket> socket;
    SocketAction action;
    std::vector<char> data;
};



class Channel;
class DataHandler {
public:
    DataHandler(Channel* channel);
    virtual void initialize(const char* data, uint32_t size) = 0;
    virtual void handleData(const char* data, uint32_t size) = 0;
    virtual void deinitialize(const char* data, uint32_t size) = 0;
private:
    Channel* _channel;
};

class ExecutionHandler : public DataHandler {
public:
    ExecutionHandler(Channel* channel);
    virtual void initialize(const char* data, uint32_t size);
    virtual void handleData(const char* data, uint32_t size);
    virtual void deinitialize(const char* data, uint32_t size);
};

class Connection;
class Channel {
public:
    Channel(uint32_t channelId, Connection* connection)
        : _channelId(channelId)
        , _connection(connection)
    {}

    void initialize(const char* data, uint32_t size);
    void deinitialize(const char* data, uint32_t size);
    void handleData(const char* data, uint32_t size);
    void sendData(const char* data, uint32_t size);
private:
    std::unique_ptr<DataHandler> createDataHandler(std::string instruction);

    uint32_t _channelId;
    Connection* _connection;
    std::unique_ptr<DataHandler> _dataHandler;

};

class Connection {
public:
    Connection(std::shared_ptr<ghoul::io::Socket> socket) {
        _socket = socket;
    }

    const ghoul::io::Socket* socket() const {
        return _socket.get();
    }

    void handleMessage(const char* data, uint32_t size);
    void sendMessage(const char* data, uint32_t size, uint32_t channelId);
    
private:
    std::map<int32_t, std::unique_ptr<Channel>> _channels;
    std::shared_ptr<ghoul::io::Socket> _socket;
};


class ServerModule : public OpenSpaceModule {
public:
    ServerModule();
    virtual ~ServerModule();
protected:
    void internalInitialize() override;
private:
    void handleSocket(std::shared_ptr<ghoul::io::Socket> socket);
    void consumeMessages();

    ConnectionPool _connectionPool;
    std::mutex _messageQueueMutex;
    std::deque<Message> _messageQueue;

    std::vector<Connection> _connections;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
