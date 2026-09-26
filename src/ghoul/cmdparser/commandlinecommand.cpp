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

#include <ghoul/cmdparser/commandlinecommand.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <utility>

namespace ghoul::cmdparser {

CommandlineCommand::CommandExecutionException::CommandExecutionException(std::string msg)
    : RuntimeError(std::move(msg), "Command")
{}

CommandlineCommand::CommandParameterException::CommandParameterException(std::string msg)
    : RuntimeError(std::move(msg), "Command")
{}

CommandlineCommand::CommandlineCommand(std::string name, std::string shortName,
                                       std::string infoText, std::string parameterList,
                                       int argumentNum, MultipleCalls allowMultipleCalls)
    : _name(std::move(name))
    , _shortName(std::move(shortName))
    , _infoText(std::move(infoText))
    , _parameterList(std::move(parameterList))
    , _nArguments(argumentNum)
    , _allowsMultipleCalls(allowMultipleCalls)
{
    ghoul_assert(!_name.empty(), "Name must not be empty");
    ghoul_assert(_name[0] == '-', "Name must start with a '-'");
    if (!_shortName.empty()) {
        ghoul_assert(_shortName[0] == '-', "Short name must start with a '-'");
    }
}

const std::string& CommandlineCommand::name() const {
    return _name;
}

const std::string& CommandlineCommand::shortName() const {
    return _shortName;
}

const std::string& CommandlineCommand::parameterList() const {
    return _parameterList;
}

const std::string& CommandlineCommand::infoText() const {
    return _infoText;
}

int CommandlineCommand::argumentNumber() const {
    return _nArguments;
}

CommandlineCommand::MultipleCalls CommandlineCommand::allowsMultipleCalls() const {
    return _allowsMultipleCalls;
}

std::string CommandlineCommand::usage() const {
    std::string result = "[";
    if (!shortName().empty()) {
        result += std::format("<{}|{}>", shortName(), name());
    }
    else {
        result += name();
    }

    if (!parameterList().empty()) {
        result += std::format(" {}", parameterList());
    }

    result += "]";

    return result;
}

std::string CommandlineCommand::help() const {
    if (!shortName().empty()) {
        return std::format("{}|{}:\t{}", shortName(), name(), infoText());
    }
    else {
        return std::format("{}:\t{}", name(), infoText());
    }
}

void CommandlineCommand::checkParameters(const std::vector<std::string>& param) const {
    if (param.size() != static_cast<size_t>(_nArguments) && _nArguments != -3) {
        throw CommandParameterException(std::format(
            "Wrong number of arguments. Expected {} got {}",
            argumentNumber(), param.size()
        ));
    }
}

} // namespace ghoul::cmdparser
