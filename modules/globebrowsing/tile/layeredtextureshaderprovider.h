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

#include <vector>
#include <array>
#include <string>
#include "ghoul/opengl/programobject.h"

#include <modules/globebrowsing/tile/layeredtextures.h>

//////////////////////////////////////////////////////////////////////////////////////////
//							  LAYERED TEXTURE SHADER PROVIDER						    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
    using namespace ghoul::opengl;

    class LayeredTextureShaderUniformIdHandler;

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

    struct LayeredTexturePreprocessingData
    {
        std::array<LayeredTextureInfo, LayeredTextures::NUM_TEXTURE_CATEGORIES> layeredTextureInfo;
        std::vector<std::pair<std::string, std::string> > keyValuePairs;
        bool operator==(const LayeredTexturePreprocessingData& other) const;
    };

    class LayeredTextureShaderProvider
    {
    public:
        LayeredTextureShaderProvider(
            const std::string& shaderName,
            const std::string& vsPath,
            const std::string& fsPath);
        ~LayeredTextureShaderProvider();

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

    class LayeredTextureShaderUniformIdHandler
    {

    public:

        static const size_t NUM_TILE_DATA_VARIABLES = 5;
        static const size_t NUM_BLEND_TEXTURES = 3;

        enum GlslTileDataId {
            textureSampler,
            depthTransform_depthScale,
            depthTransform_depthOffset,
            uvTransform_uvOffset,
            uvTransform_uvScale,
        };

        enum BlendLayerSuffix {
            none,
            Parent1,
            Parent2,
        };

        LayeredTextureShaderUniformIdHandler();
        ~LayeredTextureShaderUniformIdHandler();
        void updateIdsIfNecessary(LayeredTextureShaderProvider* shaderProvider);

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