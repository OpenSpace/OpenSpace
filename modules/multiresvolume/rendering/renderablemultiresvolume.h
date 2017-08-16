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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___RENDERABLEMULTIRESVOLUME___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___RENDERABLEMULTIRESVOLUME___H__

#include <vector>
#include <chrono>
#include <memory>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/stringproperty.h>
#include <modules/multiresvolume/rendering/multiresvolumeraycaster.h>

// Forward declare to minimize dependencies
namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
}

namespace openspace {
    
class TSP;
class AtlasManager;
class BrickSelector;
class TfBrickSelector;
class SimpleTfBrickSelector;
class LocalTfBrickSelector;
class HistogramManager;
class ErrorHistogramManager;
class LocalErrorHistogramManager;


class RenderableMultiresVolume : public Renderable {
public:
    RenderableMultiresVolume(const ghoul::Dictionary& dictionary);
    ~RenderableMultiresVolume();

    enum Selector {TF, SIMPLE, LOCAL};

    bool setSelectorType(Selector selector);
    bool initializeSelector();

    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    virtual void update(const UpdateData& data) override;
    virtual void render(const RenderData& data, RendererTasks& tasks);

  
     
    //virtual void preResolve(ghoul::opengl::ProgramObject* program) override;
    //virtual std::string getHeaderPath() override;
    //virtual std::string getHelperPath() override;
    //virtual std::vector<ghoul::opengl::Texture*> getTextures() override;
    //virtual std::vector<unsigned int> getBuffers() override;

private:
    double _time;
    double _startTime;
    double _endTime;

    properties::BoolProperty _useGlobalTime;
    properties::BoolProperty _loop;
    properties::IntProperty _currentTime; // used to vary time, if not using global time nor looping
    properties::IntProperty _memoryBudget;
    properties::IntProperty _streamingBudget;
    properties::FloatProperty _stepSizeCoefficient;
    properties::StringProperty _selectorName;
    properties::BoolProperty _statsToFile;
    properties::StringProperty _statsToFileName;

    // Stats timers
    std::string _statsFileName;
    bool _gatheringStats;
    std::chrono::system_clock::time_point _frameStart;
    std::chrono::duration<double> _selectionDuration;
    std::chrono::duration<double> _uploadDuration;
    unsigned int _nDiskReads;
    unsigned int _nUsedBricks;
    unsigned int _nStreamedBricks;

    int _timestep;

    std::string _filename;

    std::string _transferFunctionName;
    std::string _volumeName;

    std::string _transferFunctionPath;
    std::string _errorHistogramsPath;

    std::shared_ptr<TransferFunction> _transferFunction;

    float _spatialTolerance;
    float _temporalTolerance;

    std::shared_ptr<TSP> _tsp;
    std::vector<int> _brickIndices;
    int _atlasMapSize;

    std::shared_ptr<AtlasManager> _atlasManager;

    std::unique_ptr<MultiresVolumeRaycaster> _raycaster;

    TfBrickSelector* _tfBrickSelector;
    SimpleTfBrickSelector* _simpleTfBrickSelector;
    LocalTfBrickSelector* _localTfBrickSelector;

    Selector _selector;

    HistogramManager* _histogramManager;
    ErrorHistogramManager* _errorHistogramManager;
    LocalErrorHistogramManager* _localErrorHistogramManager;

    float _w;
    PowerScaledCoordinate _pscOffset;

    properties::IntProperty _scalingExponent;
    properties::Vec3Property _translation;
    properties::Vec3Property _rotation;
    properties::Vec3Property _scaling;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___RENDERABLEMULTIRESVOLUME___H__
