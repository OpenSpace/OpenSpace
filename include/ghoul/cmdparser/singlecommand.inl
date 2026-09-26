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

#include <ghoul/misc/stringhelper.h>
#include <type_traits>
#include <utility>

namespace ghoul::cmdparser {

template <typename T>
SingleCommand<T>::SingleCommand(std::optional<T>& ptr1, std::string name,
                                std::string shortName, std::string infoText,
                                std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        1,
        MultipleCalls::No
    )
    , _ptr1(ptr1)
{
    if constexpr (std::is_same_v<T, std::string>) {
        _nArguments = -3;
    }
}

template <typename T>
void SingleCommand<T>::execute(const std::vector<std::string>& parameters) {
    if constexpr (std::is_same_v<T, std::string>) {
        // If we have a string the parameter set might contain an arbitrary number of
        // parameters that we have to concatenate first
        _ptr1 = join(std::vector<std::string>(parameters.begin(), parameters.end()), " ");
    }
    else {
        _ptr1 = cast<T>(parameters[0]);
    }
}

template <typename T>
void SingleCommand<T>::checkParameters(const std::vector<std::string>& parameters) const {
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <typename T, typename U>
SingleCommand<T, U>::SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2,
                                   std::string name, std::string shortName,
                                   std::string infoText, std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        2,
        MultipleCalls::No
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
{}

template <typename T, typename U>
void SingleCommand<T, U>::execute(const std::vector<std::string>& parameters) {
    _ptr1 = cast<T>(parameters[0]);
    _ptr2 = cast<U>(parameters[1]);
}

template <typename T, typename U>
void SingleCommand<T, U>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0]) || !is<U>(parameters[1])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <typename T, typename U, typename V>
SingleCommand<T, U, V>::SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2,
                                      std::optional<V>& ptr3, std::string name,
                                      std::string shortName, std::string infoText,
                                      std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        3,
        MultipleCalls::No
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
    , _ptr3(ptr3)
{}

template <typename T, typename U, typename V>
void SingleCommand<T, U, V>::execute(const std::vector<std::string>& parameters) {
    _ptr1 = cast<T>(parameters[0]);
    _ptr2 = cast<U>(parameters[1]);
    _ptr3 = cast<V>(parameters[2]);
}

template <typename T, typename U, typename V>
void SingleCommand<T, U, V>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0]) || !is<U>(parameters[1]) || !is<V>(parameters[2])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <typename T, typename U, typename V, typename W>
SingleCommand<T, U, V, W>::SingleCommand(std::optional<T>& ptr1, std::optional<U>& ptr2,
                                         std::optional<V>& ptr3, std::optional<W>& ptr4,
                                         std::string name, std::string shortName,
                                         std::string infoText, std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        4,
        MultipleCalls::No
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
    , _ptr3(ptr3)
    , _ptr4(ptr4)
{}

template <typename T, typename U, typename V, typename W>
void SingleCommand<T, U, V, W>::execute(const std::vector<std::string>& parameters) {
    _ptr1 = cast<T>(parameters[0]);
    _ptr2 = cast<U>(parameters[1]);
    _ptr3 = cast<V>(parameters[2]);
    _ptr4 = cast<W>(parameters[3]);
}

template <typename T, typename U, typename V, typename W>
void SingleCommand<T, U, V, W>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0]) || !is<U>(parameters[1]) ||
        !is<V>(parameters[2]) || !is<W>(parameters[3]))
    {
        throw CommandParameterException("Parameter conversion failed");
    }
}

} // namespace ghoul::cmdparser
