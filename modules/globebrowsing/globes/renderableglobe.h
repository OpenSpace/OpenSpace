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

#include <modules/globebrowsing/geometry/ellipsoid.h>


#include <ghoul/misc/threadpool.h>
#include <modules/globebrowsing/other/distanceswitch.h>

#include <unordered_map>

namespace ghoul {
namespace opengl {
    class ProgramObject;
}
}


namespace openspace {

class ChunkedLodGlobe;
class TileProviderManager;

struct ReferencedBoolSelection : public properties::SelectionProperty {
    ReferencedBoolSelection(const std::string& identifier, const std::string& guiName)
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

    glm::dvec3 projectOnEllipsoid(glm::dvec3 position);
    const Ellipsoid& ellipsoid();
    float getHeight(glm::dvec3 position);
    float cameraMinHeight();
    double interactionDepthBelowEllipsoid();
    std::shared_ptr<ChunkedLodGlobe> chunkedLodGlobe();

    // Properties 
    properties::BoolProperty _isEnabled;
    properties::BoolProperty _toggleEnabledEveryFrame;
    properties::BoolProperty _performShading;
    properties::FloatProperty lodScaleFactor;
    std::vector<std::unique_ptr<ReferencedBoolSelection>> _categorySelections;
    properties::BoolProperty atmosphereEnabled;
    ReferencedBoolSelection debugSelection;
    properties::BoolProperty _saveOrThrowCamera;
    properties::BoolProperty _resetTileProviders;
    
private:
    double _interactionDepthBelowEllipsoid;

    std::string _frame;
    double _time;

    Ellipsoid _ellipsoid;

    std::shared_ptr<TileProviderManager> _tileProviderManager;
    std::shared_ptr<ChunkedLodGlobe> _chunkedLodGlobe;
    
    DistanceSwitch _distanceSwitch;

    properties::FloatProperty _cameraMinHeight;
};

}  // namespace openspace

#endif  // __RENDERABLEGLOBE_H__
