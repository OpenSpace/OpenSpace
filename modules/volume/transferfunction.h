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

#ifndef __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__
#define __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__

#include <ghoul/glm.h>
#include <modules/volume/envelope.h>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class Texture; }

namespace openspace::volume {

class TransferFunction {
public:
    TransferFunction() = default;
    TransferFunction(const std::string& string);

    bool envelopesToLua(lua_State* state);

    bool setEnvelopesFromString(const std::string& s);
    bool setEnvelopesFromLua(lua_State* lua);

    void loadEnvelopesFromFile(const std::string& path);
    void saveEnvelopesToFile(const std::string& path);

    bool operator!=(const TransferFunction& tf);
    bool hasEnvelopes() const;

    bool createTexture(ghoul::opengl::Texture& ptr);
    std::string serializedToString() const;

private:
    int _width = 1024;
    std::string _loadableFilePath;
    std::vector<Envelope> _envelopes;
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__
