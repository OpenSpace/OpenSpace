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

#ifndef __RENDERABLEGLOBE_H__
#define __RENDERABLEGLOBE_H__

#include <ghoul/logging/logmanager.h>

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>

#include <openspace/util/updatestructures.h>

#include <modules/globebrowsing/meshes/trianglesoup.h>

#include <modules/globebrowsing/chunk/chunkedlodglobe.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>

#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <modules/globebrowsing/other/threadpool.h>
#include <modules/globebrowsing/other/distanceswitch.h>

namespace ghoul {
namespace opengl {
    class ProgramObject;
}
}


namespace openspace {

    struct ReferencedBoolSelection : public properties::SelectionProperty {
        ReferencedBoolSelection::ReferencedBoolSelection(const std::string& identifier, const std::string& guiName)
            : properties::SelectionProperty(identifier, guiName) { }

        void addOption(const std::string& name, bool* ref) {
            int optionId= options().size();
            _referenceMap.insert({ optionId, ref });
            properties::SelectionProperty::addOption({ optionId, name});
        }

        void initialize() {
            // Set values in GUI to the current values of the references
            int nOptions = options().size();
            std::vector<int> selected;
            for (int i = 0; i < nOptions; ++i) {
                if (*_referenceMap[i]) {
                    selected.push_back(i);
                }
            }
            setValue(selected);

            onChange([this]() {
                int nOptions = this->options().size();
                for (int i = 0; i < nOptions; ++i) {
                    (*_referenceMap[i]) = false;
                }

                const std::vector<int>& selectedIndices = (*this);
                for (auto selectedIndex : selectedIndices) {
                    (*_referenceMap[selectedIndex]) = true;
                }
            });
        }

        std::unordered_map<int, bool* const> _referenceMap;
    };




class RenderableGlobe : public Renderable {
public:
    RenderableGlobe(const ghoul::Dictionary& dictionary);
    ~RenderableGlobe();

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

    glm::dvec3 geodeticSurfaceProjection(glm::dvec3 position);
    std::shared_ptr<ChunkedLodGlobe> chunkedLodGlobe();

    
    properties::BoolProperty mergeInvisible;
    properties::FloatProperty lodScaleFactor;
    properties::BoolProperty initChunkVisible;
    properties::BoolProperty renderSmallChunksFirst;
    properties::FloatProperty chunkHeight;

    // Layered rendering
    properties::SelectionProperty _baseLayersSelection;
    properties::SelectionProperty _nightLayersSelection;
    properties::SelectionProperty _heightMapsSelection;
    properties::SelectionProperty _waterMasksSelection;
    properties::SelectionProperty _overlaysSelection;
    
    properties::BoolProperty blendHeightMap;
    properties::BoolProperty blendColorMap;
    properties::BoolProperty blendNightTexture;
    properties::BoolProperty blendOverlay;
    properties::BoolProperty blendWaterMask;
    properties::BoolProperty atmosphereEnabled;

    ReferencedBoolSelection debugSelection;

    properties::BoolProperty levelByProjArea;
    properties::BoolProperty limitLevelByAvailableHeightData;

    


private:

    std::string _frame;

    void addToggleLayerProperties(
        LayeredTextures::TextureCategory category,
        properties::SelectionProperty& dest
    );

    void initializeToggleLayerProperties(
        LayeredTextures::TextureCategory category,
        properties::SelectionProperty& dest
        );

    double _time;

    Ellipsoid _ellipsoid;

    //std::vector<std::string> _heightMapKeys;
    //std::vector<std::string> _colorTextureKeys;

    std::shared_ptr<TileProviderManager> _tileProviderManager;
    std::shared_ptr<ChunkedLodGlobe> _chunkedLodGlobe;
    
    properties::BoolProperty _saveOrThrowCamera;

    std::vector<properties::BoolProperty> _activeColorLayers;
    std::vector<properties::BoolProperty> _activeNightLayers;
    std::vector<properties::BoolProperty> _activeOverlays;
    std::vector<properties::BoolProperty> _activeHeightMapLayers;
    std::vector<properties::BoolProperty> _activeWaterMaskLayers;


    void selectionChanged(
        properties::SelectionProperty selectionProperty,
        LayeredTextures::TextureCategory textureCategory);
    void baseLayerSelectionChanged();
    void nightLayersSelectionChanged();
    void heightMapsSelectionChanged();
    void waterMasksSelectionChanged();
    void overlaysSelectionChanged();

    DistanceSwitch _distanceSwitch;
};

}  // namespace openspace

#endif  // __RENDERABLEGLOBE_H__