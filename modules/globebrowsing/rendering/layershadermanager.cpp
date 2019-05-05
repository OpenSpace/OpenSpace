/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace openspace::globebrowsing {

bool
LayerShaderManager::LayerShaderPreprocessingData::LayerGroupPreprocessingData::operator==(
                                           const LayerGroupPreprocessingData& other) const
{
    return layerType == other.layerType &&
           blendMode == other.blendMode &&
           layerAdjustmentType == other.layerAdjustmentType &&
           lastLayerIdx == other.lastLayerIdx &&
           layerBlendingEnabled == other.layerBlendingEnabled;
}

bool LayerShaderManager::LayerShaderPreprocessingData::operator==(
                                          const LayerShaderPreprocessingData& other) const
{
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

LayerShaderManager::LayerShaderPreprocessingData
LayerShaderManager::LayerShaderPreprocessingData::get(const RenderableGlobe& globe)
{
    LayerShaderManager::LayerShaderPreprocessingData preprocessingData;

    std::shared_ptr<LayerManager> layerManager = globe.chunkedLodGlobe()->layerManager();
    for (size_t i = 0; i < layergroupid::NUM_LAYER_GROUPS; i++) {
        LayerShaderManager::LayerShaderPreprocessingData::LayerGroupPreprocessingData
            layeredTextureInfo;

        const LayerGroup& layerGroup = layerManager->layerGroup(i);
        const std::vector<std::shared_ptr<Layer>>& layers = layerGroup.activeLayers();

        // This check was implicit before;  not sure if it will fire or will be handled
        // elsewhere
        //ghoul_assert(
        //    !layerGroup.activeLayers().empty(),
        //    "If activeLayers is empty the following line will lead to an overflow"
        //);
        layeredTextureInfo.lastLayerIdx = static_cast<int>(
            layerGroup.activeLayers().size() - 1
        );
        layeredTextureInfo.layerBlendingEnabled = layerGroup.layerBlendingEnabled();

        for (const std::shared_ptr<Layer>& layer : layers) {
            layeredTextureInfo.layerType.push_back(layer->type());
            layeredTextureInfo.blendMode.push_back(layer->blendMode());
            layeredTextureInfo.layerAdjustmentType.push_back(
                layer->layerAdjustment().type()
            );
        }

        preprocessingData.layeredTextureInfo[i] = layeredTextureInfo;
    }

    const RenderableGlobe::GeneralProperties& generalProps = globe.generalProperties();
    const RenderableGlobe::DebugProperties& debugProps = globe.debugProperties();
    std::vector<std::pair<std::string, std::string>>& pairs =
        preprocessingData.keyValuePairs;

    pairs.emplace_back("useAccurateNormals",
        std::to_string(generalProps.useAccurateNormals)
    );
    pairs.emplace_back("useAtmosphere", std::to_string(generalProps.atmosphereEnabled));
    pairs.emplace_back("performShading", std::to_string(generalProps.performShading));
    pairs.emplace_back(
        "useEclipseShadows",
        std::to_string(generalProps.eclipseShadowsEnabled)
    );
    pairs.emplace_back(
        "useEclipseHardShadows",
        std::to_string(generalProps.eclipseHardShadows)
    );
    pairs.emplace_back("showChunkEdges", std::to_string(debugProps.showChunkEdges));
    pairs.emplace_back("showHeightResolution",
        std::to_string(debugProps.showHeightResolution)
    );
    pairs.emplace_back("showHeightIntensities",
        std::to_string(debugProps.showHeightIntensities)
    );
    pairs.emplace_back("defaultHeight", std::to_string(Chunk::DefaultHeight));

    return preprocessingData;
}

LayerShaderManager::LayerShaderManager(std::string shaderName, std::string vsPath,
                                       std::string fsPath)
    : _shaderName(std::move(shaderName))
    , _vsPath(std::move(vsPath))
    , _fsPath(std::move(fsPath))
{}

LayerShaderManager::~LayerShaderManager() {
    if (_programObject) {
        global::renderEngine.removeRenderProgram(_programObject.get());
        _programObject = nullptr;
    }
}

ghoul::opengl::ProgramObject* LayerShaderManager::programObject() const {
    ghoul_assert(_programObject, "Program does not exist. Needs to be compiled!");

    return _programObject.get();
}

void LayerShaderManager::recompileShaderProgram(
                                           LayerShaderPreprocessingData preprocessingData)
{
    _preprocessingData = std::move(preprocessingData);
    ghoul::Dictionary shaderDictionary;

    // Different layer types can be height layers or color layers for example.
    // These are used differently within the shaders.
    const std::array<
        LayerShaderPreprocessingData::LayerGroupPreprocessingData,
        layergroupid::NUM_LAYER_GROUPS
    >& textureTypes = _preprocessingData.layeredTextureInfo;

    for (size_t i = 0; i < textureTypes.size(); i++) {
        // lastLayerIndex must be at least 0 for the shader to compile,
        // the layer type is inactivated by setting use to false
        const std::string& groupName = layergroupid::LAYER_GROUP_IDENTIFIERS[i];
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

        // This is to avoid errors from shader preprocessor
        shaderDictionary.setValue(groupName + "0" + "LayerType", 0);

        for (int j = 0; j < textureTypes[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "LayerType",
                static_cast<int>(textureTypes[i].layerType[j])
            );
        }

        // This is to avoid errors from shader preprocessor
        shaderDictionary.setValue(groupName + "0" + "BlendMode", 0);

        for (int j = 0; j < textureTypes[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "BlendMode",
                static_cast<int>(textureTypes[i].blendMode[j])
            );
        }

        // This is to avoid errors from shader preprocessor
        std::string keyLayerAdjustmentType = groupName + "0" + "LayerAdjustmentType";
        shaderDictionary.setValue(keyLayerAdjustmentType, 0);

        for (int j = 0; j < textureTypes[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "LayerAdjustmentType",
                static_cast<int>(textureTypes[i].layerAdjustmentType[j])
            );
        }
    }

    ghoul::Dictionary layerGroupNames;
    for (int i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        layerGroupNames.setValue(
            std::to_string(i),
            layergroupid::LAYER_GROUP_IDENTIFIERS[i]
        );
    }
    shaderDictionary.setValue("layerGroups", layerGroupNames);

    // Other settings such as "useAtmosphere"
    for (const std::pair<std::string, std::string>& p : _preprocessingData.keyValuePairs)
    {
        shaderDictionary.setValue(p.first, p.second);
    }

    // Remove old program
    global::renderEngine.removeRenderProgram(_programObject.get());

    _programObject = global::renderEngine.buildRenderProgram(
        _shaderName,
        absPath(_vsPath),
        absPath(_fsPath),
        shaderDictionary
    );

    ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");

    using IgnoreError = ghoul::opengl::ProgramObject::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);
    _updatedSinceLastCall = true;
}

bool LayerShaderManager::updatedSinceLastCall() {
    const bool updated = _updatedSinceLastCall;
    _updatedSinceLastCall = false;
    return updated;
}

} // namespace openspace::globebrowsing
