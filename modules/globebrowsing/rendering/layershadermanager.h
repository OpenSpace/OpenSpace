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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_SHADER_MANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_SHADER_MANAGER___H__

#include <modules/globebrowsing/rendering/layermanager.h>

#include <array>
#include <string>

namespace ghoul {
namespace opengl {
class ProgramObject;
}
}
namespace openspace {
namespace globebrowsing {

/**
 * Settings per texture group that contains shader preprocessing information.
 */
struct LayerGroupPreprocessingData {
    int lastLayerIdx;
    bool layerBlendingEnabled;
    bool operator==(const LayerGroupPreprocessingData& other) const;
};

/**
 * Data needed for shader preprocessing before compiling a layered texture shader
 * program.
 *
 * If a <code>LayerShaderPreprocessingData</code> is compared with another it can
 * be determined wheter or not a <code>LayerShaderManager</code> needs to
 * recompile its shader program. For each <code>TextureGroup</code> there is
 * information about how many layers it has and whether or not to blend the texture
 * levels.
 */
struct LayerShaderPreprocessingData {
    std::array<LayerGroupPreprocessingData, LayerManager::NUM_LAYER_GROUPS>
        layeredTextureInfo;
    std::vector<std::pair<std::string, std::string> > keyValuePairs;
    bool operator==(const LayerShaderPreprocessingData& other) const;
};

/**
 * This class has ownership of an updated shader program for rendering tiles.
 */
class LayerShaderManager {
public:
    LayerShaderManager(
        const std::string& shaderName,
        const std::string& vsPath,
        const std::string& fsPath);
    ~LayerShaderManager();

    /**
     * Returns a pointer to a <code>ProgramObject</code> for rendering tiles.
     * \param <code>preprocessingData</code> determines wherer or not the shader
     * program needs to be re-compiled. If <code>preprocessingData</code> is different
     * from the last time this function was called the shader program will be
     * recompiled before returned.
     */
    ProgramObject* programObject(
        LayerShaderPreprocessingData preprocessingData);

    bool updatedOnLastCall();
        
private:
        
    void recompileShaderProgram(LayerShaderPreprocessingData preprocessingData);

    std::unique_ptr<ProgramObject> _programObject;
    LayerShaderPreprocessingData _preprocessingData;

    const std::string _shaderName;
    const std::string _vsPath;
    const std::string _fsPath;

    bool _updatedOnLastCall;
};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_SHADER_MANAGER___H__
