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

#include <utility>

namespace ghoul::cmdparser {

template <class T>
MultipleCommand<T>::MultipleCommand(std::vector<T>& ptr1, std::string name,
                                    std::string shortName, std::string infoText,
                                    std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        1,
        MultipleCalls::Yes
    )
    , _ptr1(ptr1)
{}

template <class T>
void MultipleCommand<T>::execute(const std::vector<std::string>& parameters) {
    _ptr1.push_back(cast<T>(parameters[0]));
}

template <class T>
void MultipleCommand<T>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <class T, class U>
MultipleCommand<T, U>::MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2,
                                       std::string name, std::string shortName,
                                       std::string infoText, std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        2,
        MultipleCalls::Yes
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
{}

template <class T, class U>
void MultipleCommand<T, U>::execute(const std::vector<std::string>& parameters) {
    _ptr1.push_back(cast<T>(parameters[0]));
    _ptr2.push_back(cast<U>(parameters[1]));
}

template <class T, class U>
void MultipleCommand<T, U>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0]) || !is<U>(parameters[1])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <class T, class U, class V>
MultipleCommand<T, U, V>::MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2,
                                          std::vector<V>& ptr3, std::string name,
                                          std::string shortName, std::string infoText,
                                          std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        3,
        MultipleCalls::Yes
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
    , _ptr3(ptr3)
{}

template <class T, class U, class V>
void MultipleCommand<T, U, V>::execute(const std::vector<std::string>& parameters) {
    _ptr1.push_back(cast<T>(parameters[0]));
    _ptr2.push_back(cast<U>(parameters[1]));
    _ptr3.push_back(cast<V>(parameters[2]));
}

template <class T, class U, class V>
void MultipleCommand<T, U, V>::checkParameters(
                                         const std::vector<std::string>& parameters) const
{
    CommandlineCommand::checkParameters(parameters);

    if (!is<T>(parameters[0]) || !is<U>(parameters[1]) || !is<V>(parameters[2])) {
        throw CommandParameterException("Parameter conversion failed");
    }
}

template <class T, class U, class V, class W>
MultipleCommand<T,U,V,W>::MultipleCommand(std::vector<T>& ptr1, std::vector<U>& ptr2,
                                          std::vector<V>& ptr3, std::vector<W>& ptr4,
                                          std::string name, std::string shortName,
                                          std::string infoText, std::string parameterList)
    : CommandlineCommand(
        std::move(name),
        std::move(shortName),
        std::move(infoText),
        std::move(parameterList),
        4,
        MultipleCalls::Yes
    )
    , _ptr1(ptr1)
    , _ptr2(ptr2)
    , _ptr3(ptr3)
    , _ptr4(ptr4)
{}

template <class T, class U, class V, class W>
void MultipleCommand<T,U,V,W>::execute(const std::vector<std::string>& parameters) {
    _ptr1.push_back(cast<T>(parameters[0]));
    _ptr2.push_back(cast<U>(parameters[1]));
    _ptr3.push_back(cast<V>(parameters[2]));
    _ptr4.push_back(cast<W>(parameters[3]));
}

template <class T, class U, class V, class W>
void MultipleCommand<T,U,V,W>::checkParameters(
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
