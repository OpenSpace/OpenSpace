/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/rendering/layershadermanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/opengl/programobject.h>

namespace openspace {
namespace globebrowsing {

bool LayerShaderManager::LayerShaderPreprocessingData::LayerGroupPreprocessingData::operator==(
    const LayerGroupPreprocessingData& other) const {
    return lastLayerIdx == other.lastLayerIdx &&
        layerBlendingEnabled == other.layerBlendingEnabled;
}

bool LayerShaderManager::LayerShaderPreprocessingData::operator==(
    const LayerShaderPreprocessingData& other) const {
    if (layeredTextureInfo.size() != other.layeredTextureInfo.size() ||
        keyValuePairs.size() != other.keyValuePairs.size()) {
        return false;
    }
    else {
        bool equal = true;
        for (size_t i = 0; i < layeredTextureInfo.size(); i++) {
            equal &= (layeredTextureInfo[i] == other.layeredTextureInfo[i]);
        }
        for (size_t i = 0; i < keyValuePairs.size(); i++) {
            equal &= (keyValuePairs[i] == other.keyValuePairs[i]);
        }
        return equal;
    }
}

LayerShaderManager::LayerShaderManager(const std::string& shaderName,
                                       const std::string& vsPath,
                                       const std::string& fsPath)
    : _shaderName(shaderName)
    , _vsPath(vsPath)
    , _fsPath(fsPath)
    , _updatedOnLastCall(false)
{}
    
LayerShaderManager::~LayerShaderManager() {
    if (_programObject) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }
}

ghoul::opengl::ProgramObject* LayerShaderManager::programObject(
                                           LayerShaderPreprocessingData preprocessingData)
{
    _updatedOnLastCall = false;
    if (!(preprocessingData == _preprocessingData) || _programObject == nullptr) {
        recompileShaderProgram(preprocessingData);
        _updatedOnLastCall = true;
    }
    return _programObject.get();
}

void LayerShaderManager::recompileShaderProgram(
                                           LayerShaderPreprocessingData preprocessingData)
{
    _preprocessingData = preprocessingData;
    ghoul::Dictionary shaderDictionary;

    // Different layer types can be height layers or color layers for example.
    // These are used differently within the shaders.
    auto textureTypes = _preprocessingData.layeredTextureInfo;
    for (size_t i = 0; i < textureTypes.size(); i++) {
        // lastLayerIndex must be at least 0 for the shader to compile,
        // the layer type is inactivated by setting use to false
        std::string groupName = LayerManager::LAYER_GROUP_NAMES[i];
        shaderDictionary.setValue(
            "lastLayerIndex" + groupName,
            glm::max(textureTypes[i].lastLayerIdx, 0)
        );
        shaderDictionary.setValue(
            "use" + groupName,
            textureTypes[i].lastLayerIdx >= 0
        );
        shaderDictionary.setValue(
            "blend" + groupName,
            textureTypes[i].layerBlendingEnabled
        );
    }

    // Other settings such as "useAtmosphere"
    auto keyValuePairs = _preprocessingData.keyValuePairs;
    for (size_t i = 0; i < keyValuePairs.size(); i++) {
        shaderDictionary.setValue(keyValuePairs[i].first, keyValuePairs[i].second);
    }

    // Remove old program
    OsEng.renderEngine().removeRenderProgram(_programObject);

    _programObject = OsEng.renderEngine().buildRenderProgram(
        _shaderName,
        _vsPath,
        _fsPath,
        shaderDictionary
    );

    ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");
    using IgnoreError = ghoul::opengl::ProgramObject::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
}

bool LayerShaderManager::updatedOnLastCall() {
    return _updatedOnLastCall;
}

} // namespace globebrowsing
} // namespace openspace
