/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace openspace::interaction {

template <class T>
T nextKeyframeObj(unsigned int index, const std::vector<T>& keyframeContainer,
                  std::function<void()> finishedCallback)
{
    if (index >= (keyframeContainer.size() - 1)) {
        if (index == (keyframeContainer.size() - 1)) {
            finishedCallback();
        }
        return keyframeContainer.back();
    }
    else if (index < keyframeContainer.size()) {
        return keyframeContainer[index];
    }
    else {
        return keyframeContainer.back();
    }
}

template <class T>
T prevKeyframeObj(unsigned int index, const std::vector<T>& keyframeContainer) {
    if (index >= keyframeContainer.size()) {
        return keyframeContainer.back();
    }
    else if (index > 0) {
        return keyframeContainer[index - 1];
    }
    else {
        return keyframeContainer.front();
    }
}

template <typename T>
T readFromPlayback(std::ifstream& stream) {
    T res;
    stream.read(reinterpret_cast<char*>(&res), sizeof(T));
    return res;
}

template <typename T>
T readFromPlayback(std::stringstream& stream) {
    T res;
    stream.read(reinterpret_cast<char*>(&res), sizeof(T));
    return res;
}

} // namespace openspace::interaction
