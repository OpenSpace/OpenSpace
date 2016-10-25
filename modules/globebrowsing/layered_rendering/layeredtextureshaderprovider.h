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

#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

#include <modules/globebrowsing/tile/gpustructs.h>
#include <modules/globebrowsing/tile/layermanager.h>

#include "ghoul/opengl/programobject.h"

#include <vector>
#include <array>
#include <string>

namespace openspace {
namespace globebrowsing {

    using namespace ghoul::opengl;

    class LayerManager;

    class LayeredTextureShaderUniformIdHandler;

    /**
    * Settings per texture category that contains shader preprocessing information.
    */
    struct LayeredTextureInfo
    {
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
    struct LayeredTexturePreprocessingData {
        LayeredTexturePreprocessingData(LayerManager* tpm = nullptr);

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

        LayeredTextureShaderUniformIdHandler();
        ~LayeredTextureShaderUniformIdHandler();
        void updateIdsIfNecessary(LayeredTextureShaderProvider* shaderProvider, LayerManager* layerManager);

        ProgramObject& programObject();
        
        GPULayerGroup* gpuLayerGroup(int i) const;
        
    private:
        std::array<std::unique_ptr<GPULayerGroup>, LayeredTextures::NUM_TEXTURE_CATEGORIES> _gpuLayerGroups;
    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __LAYERED_TEXTURE_SHADER_PROVIDER__
