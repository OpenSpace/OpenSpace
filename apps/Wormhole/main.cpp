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


#include <ghoul/opengl/ghoul_gl.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/ghoul.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>

#include <openspace/engine/windowdelegate.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderable.h>

#include <openspace/network/parallelserver.h>

#include <iostream>
#include <string>
#include <ghoul/glm.h>
#include <functional>
#include <sstream>
#include <iomanip>
#include <ios>

namespace {
    const std::string _loggerCat = "Wormhole";
}

int main(int argc, char** argv) {
    using namespace openspace;

    std::vector<std::string> arguments(argv, argv + argc);

    ghoul::cmdparser::CommandlineParser commandlineParser(
        "Wormhole",
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );

    std::stringstream defaultPassword;
    defaultPassword << std::hex << std::setfill('0') << std::setw(6) <<
        (std::hash<size_t>{}(
            std::chrono::system_clock::now().time_since_epoch().count()
        ) % 0xffffff);

    std::stringstream defaultChangeHostPassword;
    defaultChangeHostPassword << std::hex << std::setfill('0') << std::setw(6) <<
        (std::hash<size_t>{}(
            std::chrono::system_clock::now().time_since_epoch().count() + 1
        ) % 0xffffff);

    std::string portString = "";
    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            portString,
            "--port",
            "-p",
            "Sets the port to listen on"
            )
    );

    std::string password = "";
    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            password,
            "--password",
            "-l",
            "Sets the password to use"
            )
    );

    std::string changeHostPassword = "";
    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            changeHostPassword,
            "--hostpassword",
            "-h",
            "Sets the host password to use"
            )
    );

    commandlineParser.setCommandLine(arguments);
    commandlineParser.execute();

    if (password == "") {
        password = defaultPassword.str();
    }
    if (changeHostPassword == "") {
        changeHostPassword = defaultChangeHostPassword.str();
    }

    LINFO(fmt::format("Connection password: {}", password));
    LINFO(fmt::format("Host password: {}", changeHostPassword));

    int port = 25001;

    if (portString != "") {
        try {
            port = std::stoi(portString);
        }
        catch (...) {
            LERROR(fmt::format("Invalid port: {}", portString));
        }
    }

    ParallelServer server;
    server.start(port, password, changeHostPassword);
    server.setDefaultHostAddress("127.0.0.1");
    LINFO(fmt::format("Server listening to port {}", port));

    while (std::cin.get() != 'q') {}

    server.stop();
    LINFO("Server stopped");

    return 0;
};
