/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/tileprovider/interpolatetileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>

namespace openspace::globebrowsing {

InterpolateTileProvider::InterpolateTileProvider(const ghoul::Dictionary&) {
    ZoneScoped

    glGenFramebuffers(1, &fbo);
    glGenVertexArrays(1, &vaoQuad);
    glGenBuffers(1, &vboQuad);
    glBindVertexArray(vaoQuad);
    glBindBuffer(GL_ARRAY_BUFFER, vboQuad);
    tileCache = global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    // Quad for fullscreen with vertex (xy) and texture coordinates (uv)
    const GLfloat vertexData[] = {
        // x    y    u    v
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f
    };
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    // vertex coordinates at location 0
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(0);
    // texture coords at location 1
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        4 * sizeof(GLfloat),
        reinterpret_cast<void*>(sizeof(GLfloat) * 2)
    );
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    shaderProgram = global::renderEngine->buildRenderProgram(
        "InterpolatingProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/interpolate_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/interpolate_fs.glsl")
    );
}

InterpolateTileProvider::~InterpolateTileProvider() {
    glDeleteFramebuffers(1, &fbo);
    glDeleteBuffers(1, &vboQuad);
    glDeleteVertexArrays(1, &vaoQuad);
}

Tile InterpolateTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    TracyGpuZone("tile");

    // prev and next are the two tiles to interpolate between
    Tile prev = t1->tile(tileIndex);
    Tile next = t2->tile(tileIndex);
    // the tile before and the tile after the interpolation interval are loaded so the
    // interpolation goes smoother
    Tile prevprev = before->tile(tileIndex);
    Tile nextnext = future->tile(tileIndex);
    cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };

    if (!prev.texture || !next.texture) {
        return Tile{ nullptr, std::nullopt, Tile::Status::Unavailable };
    }

    // There is a previous and next texture to interpolate between so do the interpolation

    // The texture that will give the color for the interpolated texture
    ghoul::opengl::Texture* colormapTexture = singleImageProvider->ttile.texture;
    long long hkey = cache::ProviderTileHasher()(key);
    // The data for initializing the texture
    TileTextureInitData initData(
        prev.texture->dimensions().x,
        prev.texture->dimensions().y,
        prev.texture->dataType(),
        prev.texture->format(),
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );

    // Check if a tile exists for the given key in the tileCache
    // Initializing the tile that will contian the interpolated texture
    Tile ourTile;
    // The texture that will contain the interpolated image
    ghoul::opengl::Texture* writeTexture;
    if (tileCache->exist(key)) {
        // Get the tile from the tilecache
        ourTile = tileCache->get(key);
        // Use the texture from the tileCache
        writeTexture = ourTile.texture;
    }
    else {
        // Create a texture with the initialization data
        writeTexture = tileCache->texture(initData);
        // Create a tile with the texture
        ourTile = Tile{ writeTexture, std::nullopt, Tile::Status::OK };
        // Add it to the tilecache
        tileCache->put(key, initData.hashKey, ourTile);
    }

    // Saves current state
    GLint currentFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &currentFBO);
    global::renderEngine->openglStateCache().viewport(viewport);
    // Bind render texture to FBO
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        *writeTexture,
        0
    );
    glDisable(GL_BLEND);
    GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, textureBuffers);
    // Check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LERRORC("TileProvider", "Incomplete framebuffer");
    }
    // Setup our own viewport settings
    GLsizei w = static_cast<GLsizei>(writeTexture->width());
    GLsizei h = static_cast<GLsizei>(writeTexture->height());
    glViewport(0, 0, w, h);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);
    GLint id;
    glGetIntegerv(GL_CURRENT_PROGRAM, &id);
    // Activate shader and bind uniforms
    shaderProgram->activate();
    shaderProgram->setUniform("blendFactor", factor);

    ghoul::opengl::TextureUnit colormapUnit;
    colormapUnit.activate();
    colormapTexture->bind();
    shaderProgram->setUniform("colormapTexture", colormapUnit);

    ghoul::opengl::TextureUnit prevUnit;
    prevUnit.activate();
    prev.texture->bind();
    shaderProgram->setUniform("prevTexture", prevUnit);

    ghoul::opengl::TextureUnit nextUnit;
    nextUnit.activate();
    next.texture->bind();
    shaderProgram->setUniform("nextTexture", nextUnit);

    // Render to the texture
    glBindVertexArray(vaoQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6); // 2 triangles
    // Deactivate shader program (when rendering is completed)
    shaderProgram->deactivate();
    glUseProgram(id);
    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, currentFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetColorState();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
    global::renderEngine->openglStateCache().resetViewportState();

    return ourTile;
}

Tile::Status InterpolateTileProvider::tileStatus(const TileIndex& index) {
    Tile::Status t1Stat = t1->tileStatus(index);
    Tile::Status t2Stat = t2->tileStatus(index);
    if (t1Stat <= t2Stat) {
        return t1Stat;
    }
    else {
        return t2Stat;
    }
}

TileDepthTransform InterpolateTileProvider::depthTransform() {
    return t1->depthTransform();
}

void InterpolateTileProvider::update() {
    t1->update();
    t2->update();
    before->update();
    future->update();
}

void InterpolateTileProvider::reset() {
    t1->reset();
    t2->reset();
    before->reset();
    future->reset();
}

int InterpolateTileProvider::maxLevel() {
    return glm::min(t1->maxLevel(), t2->maxLevel());
}

float InterpolateTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
