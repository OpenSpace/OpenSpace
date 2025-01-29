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

#include <modules/debugging/rendering/screenspacedebugplane.h>

#include <openspace/documentation/documentation.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The OpenGL name of the texture that is displayed on this plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `ScreenSpaceRenderable` can be used for debugging OpenGL textures. It renders
    // the content of an existing texture, based on a provided OpenGL texture name.
    struct [[codegen::Dictionary(ScreenSpaceDebugPlane)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::optional<int> texture;
    };
#include "screenspacedebugplane_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceDebugPlane::Documentation() {
    return codegen::doc<Parameters>("debugging_screenspace_debugplane");
}

ScreenSpaceDebugPlane::ScreenSpaceDebugPlane(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _texture(TextureInfo, -1, -1, 4096)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texture = p.texture.value_or(_texture);
    addProperty(_texture);

    _objectSize = glm::ivec2(256);
}

void ScreenSpaceDebugPlane::bindTexture() {
    glBindTexture(GL_TEXTURE_2D, _texture);
}

} // namespace openspace
