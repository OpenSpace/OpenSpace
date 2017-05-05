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


#include <memory>
#include <thread>
#include <mutex>
#include <cstdint>

namespace openspace {


class ChannelHandler {

};

enum class ChannelAction : uint8_t {
    Initialize = 0,
    Data = 1,
    Deinitialize = 2
};


class ServerModule : public OpenSpaceModule {
public:
    ServerModule();
    virtual ~ServerModule() = default;
protected:
    void internalInitialize() override;
private:
    ConnectionPool _connectionPool;
    //ghoul::TemplateFactory<ChannelHandler> factory;

    void handleSocket(std::shared_ptr<ghoul::io::TcpSocket> socket);
   
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
