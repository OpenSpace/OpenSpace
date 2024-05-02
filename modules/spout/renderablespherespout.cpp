/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/spout/renderablespherespout.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/sphere.h>
#include <ghoul/opengl/texture.h>

// The RenderableSphereSpout can be used to render a sphere with a texture that is
// provided by another application on the same computer using the SPOUT library. Note that
// this library is only available on Windows.

namespace {
    struct [[codegen::Dictionary(RenderableSphereSpout)]] Parameters {
        // Specifies the GUI name of the RenderableSphereSpout
        std::optional<std::string> name;
    };
#include "renderablespherespout_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphereSpout::Documentation() {
    return codegen::doc<Parameters>(
        "spout_renderablespherespout",
        spout::SpoutReceiverPropertyProxy::Documentation()
    );
}

RenderableSphereSpout::RenderableSphereSpout(const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _spoutReceiver(*this, dictionary)
{
    codegen::bake<Parameters>(dictionary);

    int iIdentifier = 0;
    if (_identifier.empty()) {
        static int id = 0;
        iIdentifier = id;

        if (iIdentifier == 0) {
            setIdentifier("RenderableSphereSpout");
        }
        else {
            setIdentifier("RenderableSphereSpout" + std::to_string(iIdentifier));
        }
        id++;
    }

    if (_guiName.empty()) {
        // Adding an extra space to the user-facing name as it looks nicer
        setGuiName("RenderableSphereSpout " + std::to_string(iIdentifier));
    }
}

void RenderableSphereSpout::deinitializeGL() {
    _spoutReceiver.release();
    RenderableSphere::deinitializeGL();
}

void RenderableSphereSpout::update(const UpdateData& data) {
    RenderableSphere::update(data);
    _spoutReceiver.updateReceiver();
}

void RenderableSphereSpout::bindTexture() {
    if (_spoutReceiver.isReceiving()) {
        _spoutReceiver.saveGLTextureState();
        glBindTexture(GL_TEXTURE_2D, _spoutReceiver.spoutTexture());
    }
    else {
        RenderableSphere::unbindTexture();
    }
}

void RenderableSphereSpout::unbindTexture() {
    if (_spoutReceiver.isReceiving()) {
        _spoutReceiver.restoreGLTextureState();
    }
    else {
        RenderableSphere::unbindTexture();
    }
}

} // namespace openspace

#endif // WIN32
