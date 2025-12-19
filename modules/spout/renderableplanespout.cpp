/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifdef WIN32

#include <modules/spout/renderableplanespout.h>

#include <openspace/documentation/documentation.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/format.h>
#include <ghoul/misc/dictionary.h>

namespace {
    // This `Renderable` type can be used to render a plane with a texture that is
    // provided by another application on the same computer using the SPOUT library.
    // Note: The Spout library is only available on Windows.
    struct [[codegen::Dictionary(RenderablePlaneSpout)]] Parameters {
    };
#include "renderableplanespout_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneSpout::Documentation() {
    return codegen::doc<Parameters>(
        "spout_renderableplanespout",
        spout::SpoutReceiverPropertyProxy::Documentation()
    );
}

RenderablePlaneSpout::RenderablePlaneSpout(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _spoutReceiver(*this, dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    int iIdentifier = 0;
    if (_identifier.empty()) {
        static int id = 0;
        iIdentifier = id;

        if (iIdentifier == 0) {
            setIdentifier("RenderablePlaneSpout");
        }
        else {
            setIdentifier(std::format("RenderablePlaneSpout{}", iIdentifier));
        }
        id++;
    }

    if (_guiName.empty()) {
        // Adding an extra space to the user-facing name as it looks nicer
        setGuiName(std::format("RenderablePlaneSpout {}", iIdentifier));
    }
}

void RenderablePlaneSpout::deinitializeGL() {
    _spoutReceiver.release();

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneSpout::update(const UpdateData& data) {
    RenderablePlane::update(data);
    _spoutReceiver.updateReceiver();
}

void RenderablePlaneSpout::bindTexture() {
    if (_spoutReceiver.isReceiving()) {
        _spoutReceiver.saveGLTextureState();
        glBindTexture(GL_TEXTURE_2D, static_cast<GLuint>(_spoutReceiver.spoutTexture()));
    }
    else {
        RenderablePlane::bindTexture();
    }
}

void RenderablePlaneSpout::unbindTexture() {
    if (_spoutReceiver.isReceiving()) {
        _spoutReceiver.restoreGLTextureState();
    }
    else {
        RenderablePlane::unbindTexture();
    }
}

} // namespace openspace

#endif // WIN32
