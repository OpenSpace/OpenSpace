/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <chrono>
#include <filesystem>

namespace ghoul { class Dictionary; }
namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class AtlasManager;
class BrickSelector;
class ErrorHistogramManager;
class HistogramManager;
class LocalErrorHistogramManager;
class LocalTfBrickSelector;
class MultiresVolumeRaycaster;
class SimpleTfBrickSelector;
class TfBrickSelector;
class TransferFunction;
class TSP;

class RenderableMultiresVolume : public Renderable {
public:
    RenderableMultiresVolume(const ghoul::Dictionary& dictionary);
    ~RenderableMultiresVolume();

    enum Selector {
        TF,
        SIMPLE,
        LOCAL
    };

    void setSelectorType(Selector selector);
    bool initializeSelector();

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    virtual void update(const UpdateData& data) override;
    virtual void render(const RenderData& data, RendererTasks& tasks) override;

    //virtual void preResolve(ghoul::opengl::ProgramObject* program) override;
    //virtual std::string getHeaderPath() override;
    //virtual std::string getHelperPath() override;
    //virtual std::vector<ghoul::opengl::Texture*> getTextures() override;
    //virtual std::vector<unsigned int> getBuffers() override;

private:
    properties::BoolProperty _useGlobalTime;
    properties::BoolProperty _loop;
    // used to vary time, if not using global time nor looping
    properties::IntProperty _currentTime;
    properties::IntProperty _memoryBudget;
    properties::IntProperty _streamingBudget;
    properties::FloatProperty _stepSizeCoefficient;
    properties::StringProperty _selectorName;
    properties::BoolProperty _statsToFile;
    properties::StringProperty _statsToFileName;
    properties::IntProperty _scalingExponent;
    properties::Vec3Property _translation;
    properties::Vec3Property _rotation;
    properties::Vec3Property _scaling;

    double _time;
    double _startTime;
    double _endTime;


    // Stats timers
    std::string _statsFileName;
    bool _gatheringStats = false;
    std::chrono::system_clock::time_point _frameStart;
    std::chrono::duration<double> _selectionDuration;
    std::chrono::duration<double> _uploadDuration;
    unsigned int _nDiskReads;
    unsigned int _nUsedBricks;
    unsigned int _nStreamedBricks;

    int _timestep = 0;

    std::filesystem::path _filename;

    std::string _transferFunctionName;
    std::string _volumeName;

    std::filesystem::path _transferFunctionPath;
    std::filesystem::path _errorHistogramsPath;

    std::shared_ptr<TransferFunction> _transferFunction;

    std::shared_ptr<TSP> _tsp;
    std::vector<int> _brickIndices;

    std::shared_ptr<AtlasManager> _atlasManager;

    std::unique_ptr<MultiresVolumeRaycaster> _raycaster;

    std::unique_ptr<TfBrickSelector> _tfBrickSelector;
    std::unique_ptr<SimpleTfBrickSelector> _simpleTfBrickSelector;
    std::unique_ptr<LocalTfBrickSelector> _localTfBrickSelector;

    Selector _selector;

    std::unique_ptr<HistogramManager> _histogramManager;
    std::unique_ptr<ErrorHistogramManager> _errorHistogramManager;
    std::unique_ptr<LocalErrorHistogramManager> _localErrorHistogramManager;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___RENDERABLEMULTIRESVOLUME___H__
