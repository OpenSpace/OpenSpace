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

#include <modules/debugging/rendering/renderabledebugplane.h>

#include <openspace/documentation/documentation.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The OpenGL name of the texture that is displayed on this plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableDebugPlane)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::optional<int> texture;
    };
#include "renderabledebugplane_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableDebugPlane::Documentation() {
    return codegen::doc<Parameters>(
        "debugging_renderable_debugplane",
        RenderablePlane::Documentation()
    );
}

RenderableDebugPlane::RenderableDebugPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texture(TextureInfo, -1, -1, 512)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texture = p.texture.value_or(_texture);
    addProperty(_texture);
}

bool RenderableDebugPlane::isReady() const {
    return _texture >= 0 && RenderablePlane::isReady();
}

void RenderableDebugPlane::bindTexture() {
    glBindTexture(GL_TEXTURE_2D, _texture);
}

} // namespace openspace
