/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/cmdparser/commandlineparser.h>

#include <ghoul/cmdparser/commandlinecommand.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <map>
#include <sstream>
#include <string_view>
#include <utility>

namespace {
    using namespace ghoul;

    constexpr std::string_view _loggerCat = "CommandlineParser";

    struct CommandlineParserError final : public RuntimeError {
        explicit CommandlineParserError(std::string msg)
            : RuntimeError(std::move(msg), "CommandlineParser")
        {}
    };

    /**
     * Extracts multiple arguments from a single list.
     *
     * If `count` is `-2`, arguments will be extracted, as long as no new commands is
     * found. `in[begin]` itself will not be extracted if this is the command.
     *
     * If `count` is `-1`, the rest of the line will be extracted.
     * If `count` is `> 0`, that many arguments will be extracted and returned.
     */
    int extractArguments(const std::vector<std::string>& in,
                         std::vector<std::string>& out, size_t begin, int count)
    {
        if (count == -1) {
            for (size_t i = 0; (i < in.size()) && ((begin + 1 + i) < in.size()); i++) {
                out.push_back(in[begin + 1 + i]);
            }
        }
        else if (count == -2) {
            // Extract arguments until a new command is found
            // The '-' restriction is enforced in the #addCommand method
            for (size_t i = begin; (i < in.size()) && (in[i][0] != '-'); i++) {
                out.push_back(in[i]);
            }
        }
        else if (count == -3) {
            // Extract arguments until a new command is found
            // The '-' restriction is enforced in the #addCommand method
            for (size_t i = begin + 1; (i < in.size()) && (in[i][0] != '-'); i++) {
                out.push_back(in[i]);
            }
        }
        else {
            for (int i = 0; (i < count) && ((begin + 1 + i) < in.size()); i++) {
                out.push_back(in[begin + 1 + i]);
            }
        }

        return static_cast<int>(out.size());
    }
} // namespace

namespace ghoul::cmdparser {

CommandlineParser::CommandlineParser(std::string programName,
                                     AllowUnknownCommands allowUnknownCommands,
                                     std::string shortHelpCommand,
                                     std::string longHelpCommand)
    : _programName(std::move(programName))
    , _allowUnknownCommands(allowUnknownCommands)
    , _shortHelpCommand(std::move(shortHelpCommand))
    , _longHelpCommand(std::move(longHelpCommand))
{}

void CommandlineParser::setAllowUnknownCommands(AllowUnknownCommands allowUnknownCommands)
{
    _allowUnknownCommands = allowUnknownCommands;
}

CommandlineParser::AllowUnknownCommands CommandlineParser::allowsUnknownCommands() const {
    return _allowUnknownCommands;
}

const std::string& CommandlineParser::programPath() const {
    return _programPath;
}

const std::vector<std::string>& CommandlineParser::setCommandLine(
                                                       std::vector<std::string> arguments)
{
    // arguments[0] = program name
    // arguments[i] = i-th argument
    ghoul_assert(!arguments.empty(), "Arguments must not be empty");
    _programPath = arguments.front();

    // Might be possible that someone calls us multiple times
    _arguments.clear();
    _remainingArguments.clear();

    // Just add the arguments to the vector
    _arguments.assign(arguments.begin() + 1, arguments.end());

    return _remainingArguments;
}

CommandlineParser::DisplayHelpText CommandlineParser::execute() {
    if (_arguments.empty()) {
        return DisplayHelpText::No;
    }

    // There is only one argument and this is either "-h" or "--help" so display the help
    if (hasOnlyHelpCommand()) {
        return DisplayHelpText::Yes;
    }

    std::vector<std::string> argumentsForNameless;

    // We store the commands and parameters in a map to be able to execute them without
    // parsing the commandline again
    std::multimap<CommandlineCommand*, std::vector<std::string>> parameterMap;

    for (size_t i = 0 ; i < _arguments.size();) {
        // In the beginning we assume that we just started the loop or finished reading
        // parameters for the last command

        // Test if the next argument is a command or a parameter for a nameless argument.
        // The restriction for '-' is enforced in the #addCommand method
        if (_arguments[i][0] != '-') {
            // The rest of the commands until the next '-' are for the nameless command if
            // we have one
            if (_commandForNamelessArguments) {
                const int number = extractArguments(
                    _arguments,
                    argumentsForNameless,
                    i,
                    -2
                );
                i += (number - 1);
            }
            else {
                // If we do not have a command for nameless arguments, but we have a place
                // to store them; we do not need to check if the nameless command is
                // available as this is done later
                if (_allowUnknownCommands) {
                    std::vector<std::string> arguments;
                    const int number = extractArguments(_arguments, arguments, i, -2);
                    for (const std::string& arg : arguments) {
                        _remainingArguments.push_back(arg);
                    }
                    i += number;
                }
                else {
                    // We have found an unknown command but we don't allow them, so we
                    // have to bail out here
                    throw CommandlineParserError(std::format(
                        "Found unknown command '{}' but none are allowed",
                        _arguments[i]
                    ));
                }
            }
        }
        else {
            // We have found a command
            CommandlineCommand* currentCmd = getCommand(_arguments[i]);

            // currentCommand = nullptr, if there wasn't a command with that specific name
            // or shortName
            if (!currentCmd) {
                if (_allowUnknownCommands) {
                    // Extract the rest of the arguments
                    std::vector<std::string> arguments;
                    const int number = extractArguments(_arguments, arguments, i + 1, -2);
                    _remainingArguments.push_back(_arguments[i]);
                    for (const std::string& arg : arguments) {
                        _remainingArguments.push_back(arg);
                    }
                    i += number + 1;
                    continue;
                }
                else {
                    throw CommandlineParserError(std::format(
                        "{} is not a valid command", _arguments[i]
                    ));
                }
            }

            std::vector<std::string> params;
            const int n = extractArguments(
                _arguments,
                params,
                i,
                currentCmd->argumentNumber()
            );
            i += (n + 1);

            // Don't insert if the command doesn't allow multiple calls and already is in
            // the map
            if (!currentCmd->allowsMultipleCalls() &&
                parameterMap.find(currentCmd) != parameterMap.end())
            {
                throw CommandlineParserError(std::format(
                    "'{}' does not allow multiple calls in a single line",
                    currentCmd->name()
                ));
            }

            parameterMap.emplace(currentCmd, params);
        }
    }
    // We now have all the commands with the respective parameters stored in the map and
    // all the parameters for the nameless command is available as well.

    // First step: Test, if we have nameless arguments even if we don't have a nameless
    // command. If so, bail out
    if (!argumentsForNameless.empty() && (!_commandForNamelessArguments)) {
        if (_allowUnknownCommands) {
            _remainingArguments.insert(
                _remainingArguments.end(),
                argumentsForNameless.begin(),
                argumentsForNameless.end()
            );
        }
        else {
            throw CommandlineParserError("No command available for nameless parameters");
        }
    }

    // Second step: Check if every command is happy with the parameters assigned to it
    using K = CommandlineCommand*;
    using V = std::vector<std::string>;
    for (const std::pair<const K, V>& p : parameterMap) {
        p.first->checkParameters(p.second);
    }

    // Second-and-a-half step: Display pairs for (command,argument) in debug level
    std::stringstream s;
    for (const std::string& arg : argumentsForNameless) {
        s << " " << arg;
    }
    LDEBUG(std::format("(Nameless argument: {})", s.str()));

    for (const std::pair<const K, V>& it : parameterMap) {
        s.clear();
        for (const std::string& arg : it.second) {
            s << " " << arg;
        }
        LDEBUG(std::format("({}: {})", it.first->name(), s.str()));
    }

    // Third step: Execute the nameless command if there are any arguments available
    if (!argumentsForNameless.empty()) {
        _commandForNamelessArguments->checkParameters(argumentsForNameless);
        _commandForNamelessArguments->execute(argumentsForNameless);
    }

    // Fourth step: Execute the commands (this step is only done if everyone is happy up
    // until now)
    for (const std::pair<const K, V>& it : parameterMap) {
        it.first->execute(it.second);
    }

    // If we made it this far it means that all commands have been executed successfully
    return DisplayHelpText::No;
}

void CommandlineParser::addCommand(std::unique_ptr<CommandlineCommand> cmd) {
    ghoul_assert(cmd, "Command must not be nullptr");
    ghoul_assert(!getCommand(cmd->name()), "Name was previously registered");

    if (!cmd->shortName().empty()) {
        ghoul_assert(!getCommand(cmd->shortName()), "Shortname registered previously");
    }

    _commands.push_back(std::move(cmd));
}

void CommandlineParser::addCommandForNamelessArguments(
                                                  std::unique_ptr<CommandlineCommand> cmd)
{
    ghoul_assert(cmd, "Command must not be empty");
    _commandForNamelessArguments = std::move(cmd);
}

bool CommandlineParser::hasCommandForName(const std::string& name) const {
    const auto it = std::find_if(
        _commands.cbegin(),
        _commands.cend(),
        [&name](const std::unique_ptr<CommandlineCommand>& c) {
            return c->name() == name;
        }
    );
    return it != _commands.cend();
}

bool CommandlineParser::hasCommandForShortName(const std::string& shortName) const {
    const auto it = std::find_if(
        _commands.cbegin(),
        _commands.cend(),
        [&shortName](const std::unique_ptr<CommandlineCommand>& command) {
            return command->shortName() == shortName;
        }
    );
    return it != _commands.cend();
}

bool CommandlineParser::hasNamelessCommand() const {
    return _commandForNamelessArguments != nullptr;
}

std::string CommandlineParser::usageInformation() const {
    std::string result;
    if (!_programName.empty()) {
        result += _programName + " ";
    }

    if (_commandForNamelessArguments) {
        result += _commandForNamelessArguments->usage() + " ";
    }

    for (const std::unique_ptr<CommandlineCommand>& it : _commands) {
        result += "\n" + it->usage() + " ";
    }

    return result;
}

std::string CommandlineParser::usageInformationForCommand(
                                                         const std::string& command) const
{
    ghoul_assert(!command.empty(), "Command must not be empty");

    const auto it = std::find_if(
        _commands.cbegin(),
        _commands.cend(),
        [command](const std::unique_ptr<CommandlineCommand>& i) {
            return i->name() == command || i->shortName() == command;
        }
    );
    ghoul_assert(it != _commands.cend(), "Command must name a valid name or shortname");

    return std::format("Usage: \n{}", (*it)->usage());
}

std::string CommandlineParser::usageInformationForNamelessCommand() const {
    ghoul_assert(
        _commandForNamelessArguments,
        "A nameless commandline argument must be registered"
    );
    return "Usage: \n" + _commandForNamelessArguments->usage();
}

std::string CommandlineParser::helpText() const {
    std::string result = std::format("{}\n\nHelp:\n-----\n", usageInformation());
    for (const std::unique_ptr<CommandlineCommand>& it : _commands) {
        result += it->help() + '\n';
    }
    return result;
}

CommandlineCommand* CommandlineParser::getCommand(const std::string& shortOrLongName) {
    if (shortOrLongName.empty()) {
        return nullptr;
    }
    const auto it = std::find_if(
        _commands.cbegin(),
        _commands.cend(),
        [shortOrLongName](const std::unique_ptr<CommandlineCommand>& i) {
            return i->name() == shortOrLongName || i->shortName() == shortOrLongName;
        }
    );
    return it != _commands.cend() ? it->get() : nullptr;
}

bool CommandlineParser::hasOnlyHelpCommand() const {
    return (
        (_arguments.size() == 1) &&
        ((_arguments[0] == _shortHelpCommand) || (_arguments[0] == _longHelpCommand))
    );
}

} // namespace ghoul::cmdparser
