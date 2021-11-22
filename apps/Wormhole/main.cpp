/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/network/parallelserver.h>
#include <ghoul/fmt.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/logging/logmanager.h>
#include <iomanip>

namespace {
    constexpr const char*_loggerCat = "Wormhole";
} // namespace

int main(int argc, char** argv) {
    using namespace openspace;
    using namespace ghoul::cmdparser;

    std::vector<std::string> arguments(argv, argv + argc);

    CommandlineParser commandlineParser(
        "Wormhole",
        CommandlineParser::AllowUnknownCommands::Yes
    );

    struct {
        std::string port;
        std::string password;
        std::string changeHostPassword;
    } settings;

    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            settings.port,
            "--port",
            "-p",
            "Sets the port to listen on"
        )
    );

    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            settings.password,
            "--password",
            "-l",
            "Sets the password to use"
        )
    );

    commandlineParser.addCommand(
        std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
            settings.changeHostPassword,
            "--hostpassword",
            "-h",
            "Sets the host password to use"
        )
    );

    commandlineParser.setCommandLine(arguments);
    commandlineParser.execute();

    if (settings.password.empty()) {
        std::stringstream defaultPassword;
        defaultPassword << std::hex << std::setfill('0') << std::setw(6) <<
            (std::hash<size_t>{}(
                std::chrono::system_clock::now().time_since_epoch().count()
                ) % 0xffffff);

        settings.password = defaultPassword.str();
    }
    if (settings.changeHostPassword.empty()) {
        std::stringstream defaultChangeHostPassword;
        defaultChangeHostPassword << std::hex << std::setfill('0') << std::setw(6) <<
            (std::hash<size_t>{}(
                std::chrono::system_clock::now().time_since_epoch().count() + 1
                ) % 0xffffff);

        settings.changeHostPassword = defaultChangeHostPassword.str();
    }
    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );
    LINFO(fmt::format("Connection password: {}", settings.password));
    LINFO(fmt::format("Host password: {}", settings.changeHostPassword));

    int port = 25001;

    if (!settings.port.empty()) {
        try {
            port = std::stoi(settings.port);
        }
        catch (const std::invalid_argument&) {
            LERROR(fmt::format("Invalid port: {}", settings.port));
        }
    }

    ParallelServer server;
    server.start(port, settings.password, settings.changeHostPassword);
    server.setDefaultHostAddress("127.0.0.1");
    LINFO(fmt::format("Server listening to port {}", port));

    while (std::cin.get() != 'q') {
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    }

    server.stop();
    LINFO("Server stopped");

    return 0;
}
