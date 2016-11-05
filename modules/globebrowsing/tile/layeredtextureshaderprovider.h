/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __LAYERED_TEXTURE_SHADER_PROVIDER__
#define __LAYERED_TEXTURE_SHADER_PROVIDER__

#include <modules/globebrowsing/tile/layeredtextures.h>

#include "ghoul/opengl/programobject.h"

#include <vector>
#include <array>
#include <string>

//////////////////////////////////////////////////////////////////////////////////////////
//                              LAYERED TEXTURE SHADER PROVIDER                         //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
    using namespace ghoul::opengl;

    class LayeredTextureShaderUniformIdHandler;

    /**
    * Settings per texture category that contains shader preprocessing information.
    */
    struct LayeredTextureInfo
    {
        static const size_t NUM_SETTINGS_PER_CATEGORY = 3;
        enum GlslKeyPrefixes
        {
            lastLayerIndex,
            use,
            blend,
        };
        static const std::string glslKeyPrefixes[NUM_SETTINGS_PER_CATEGORY];
  
        int lastLayerIdx;
        bool layerBlendingEnabled;

        bool operator==(const LayeredTextureInfo& other) const;
    };

    /**
    * Data needed for shader preprocessing before compiling a layered texture shader
    * program.
    *
    * If a <code>LayeredTexturePreprocessingData</code> is compared with another it can
    * be determined wheter or not a <code>LayeredTextureShaderProvider</code> needs to
    * recompile its shader program. For each <code>TextureCategory</code> there is
    * information about how many layers it has and whether or not to blend the texture
    * levels.
    */
    struct LayeredTexturePreprocessingData
    {
        std::array<LayeredTextureInfo, LayeredTextures::NUM_TEXTURE_CATEGORIES>
            layeredTextureInfo;
        std::vector<std::pair<std::string, std::string> > keyValuePairs;
        bool operator==(const LayeredTexturePreprocessingData& other) const;
    };

    /**
    * This class has ownership of an updated shader program for rendering tiles.
    */
    class LayeredTextureShaderProvider
    {
    public:
        LayeredTextureShaderProvider(
            const std::string& shaderName,
            const std::string& vsPath,
            const std::string& fsPath);
        ~LayeredTextureShaderProvider();

        /**
        * Returns a pointer to a <code>ProgramObject</code> for rendering tiles.
        * \param <code>preprocessingData</code> determines wherer or not the shader
        * program needs to be re-compiled. If <code>preprocessingData</code> is different
        * from the last time this function was called the shader program will be
        * recompiled before returned.
        */
        ProgramObject* getUpdatedShaderProgram(
            LayeredTexturePreprocessingData preprocessingData);

        bool updatedOnLastCall();
    private:
        friend class LayeredTextureShaderUniformIdHandler;
        void recompileShaderProgram(LayeredTexturePreprocessingData preprocessingData);

        std::unique_ptr<ProgramObject> _programObject;
        LayeredTexturePreprocessingData _preprocessingData;

        const std::string _shaderName;
        const std::string _vsPath;
        const std::string _fsPath;

        bool _updatedOnLastCall;
    };

    /**
    * This class caches OpenGL uniform IDs for <code>LayeredTextureShaderProvider</code>s.
    */
    class LayeredTextureShaderUniformIdHandler
    {
    public:
        static const size_t NUM_TILE_DATA_VARIABLES = 5;
        static const size_t NUM_BLEND_TEXTURES = 3;

        /**
        * Each texture can have these uniform variables associated with it in the shader
        * code.
        *
        * <code>textureSampler</code> is the actual texture that can be sampled in the
        * shader program. The associated GLSL type is <code>sampler2D</code>.
        * <code>depthTransform_depthScale</code> specifies the scale part of the depth
        * transform. Useful for height maps. The associated GLSL type is
        * <code>float</code>.
        * <code>depthTransform_depthOffset</code> specifies the offset part of the depth
        * transform. Useful for height maps. The associated GLSL type is
        * <code>float</code>.
        * <code>uvTransform_uvOffset</code> specifies an offset that can be used when
        * sampling from the texture. The associated GLSL type is <code>vec2</code>.
        * <code>uvTransform_uvScale</code> specifies a scale that can be used when
        * sampling from the texture. The associated GLSL type is <code>vec2</code>.
        *
        * The corresponding struct in GLSL code for storing these data is a
        * <code>Tile</code>. The names of the uniforms are the ones specified in 
        * <code>glslTileDataNames</code>.
        */
        enum GlslTileDataId {
            textureSampler,
            depthTransform_depthScale,
            depthTransform_depthOffset,
            uvTransform_uvOffset,
            uvTransform_uvScale,
        };

        /**
        * These suffixes are used when naming <code>Tile</code>s in GLSL code. The names
        * of the <code>Tile</code>s is one of
        * <code>LayeredTextures::TEXTURE_CATEGORY_NAMES</code> followed by the suffixes
        * defined in <code>blendLayerSuffixes</code>.
        */
        enum BlendLayerSuffixes {
            none,
            Parent1,
            Parent2,
        };

        LayeredTextureShaderUniformIdHandler();
        ~LayeredTextureShaderUniformIdHandler();
        void updateIdsIfNecessary(LayeredTextureShaderProvider* shaderProvider);

        /**
        * \param <code>category</code> can be one of the categories specified in
        * <code>LayeredTextures::TextureCategory</code>.
        * \param <code>blendLayer</code> can have a value between 0 and
        * <code>NUM_BLEND_TEXTURES</code> and specified that it is the  uniform of that
        * specific blend layer that is requested.
        * \param <code>layerIndex</code> should have a value between 0 and
        * <code>LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY - 1</code> and will specify
        * which of the texture layers that is requested.
        * \param <code>tileDataId</code> specifies what variable for the texture that
        * should be returned. It can have any of the values specified in
        * <code>GlslTileDataId</code>.
        *
        * \returns an OpenGL uniform ID for the specified arguments. If the uniform does
        * not exist in the shader program it returns -1.
        */
        GLint getId(
            LayeredTextures::TextureCategory category,
            size_t blendLayer,
            size_t layerIndex,
            GlslTileDataId tileDataId);
        ProgramObject& programObject();
    private:
        static const std::string glslTileDataNames[NUM_TILE_DATA_VARIABLES];
        static const std::string blendLayerSuffixes[NUM_BLEND_TEXTURES];

        std::array<
            std::array<
            std::array<
            std::array<
            GLint,
            NUM_TILE_DATA_VARIABLES>,
            LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY>,
            NUM_BLEND_TEXTURES>,
            LayeredTextures::NUM_TEXTURE_CATEGORIES>
            _tileUniformIds;

        LayeredTextureShaderProvider* _shaderProvider;
    };
}  // namespace openspace

#endif  // __LAYERED_TEXTURE_SHADER_PROVIDER__