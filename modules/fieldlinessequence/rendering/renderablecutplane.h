//
//  renderablecutplane.hpp
//  openspace-module-base
//
//  Created by Julia Johnstone on 2023-05-08.
//

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__

#include <modules/base/rendering/renderableplane.h>
#include <modules/fieldlinessequence/util/gameravolumeslicer.h>

#include <string>
#include <iostream>
#include <ghoul/glm.h>
#include <glm/vec2.hpp>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <openspace/rendering/transferfunction.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class Texture; }

namespace openspace {

struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableCutPlane : public RenderablePlane {
public:
    RenderableCutPlane(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;
    
    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();
    
protected:
    virtual void bindTexture() override;
    void createPlane();
    
private:
    void loadDataFromSlice();
    std::unique_ptr<ghoul::opengl::Texture> createTexture(const std::vector<std::vector<float>>& data);

    // Axis to cut the volume on 
    std::string _axis;
    // Value to slice on
    float _cutValue;
    // What data property to render
    std::string _dataProperty;
    // Index of the data property 
    int _dataPropertyIndex;
    // Path to volume data file
    properties::StringProperty _filePath;
    // Size unit (in meters) of the axes dimensions
    properties::Vec3Property _size;
    // Paths to color tables 
    std::vector<std::string> _colorTablePaths;
    // Values represents min & max values represented in the color table
    std::vector<glm::vec2> _colorTableRanges;

    std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
    std::unique_ptr<TransferFunction> _transferFunction;

    GameraVolumeSlicer _slice;
    //The axis dimensions of the axis we're not slicing on
    std::vector<std::vector<float>> _axisDim;


    int _xAxisLength;
    int _yAxisLength;
    int _zAxisLength;
    // Align 
    double _alignOnX;
    double _alignOnY;
    double _alignOnZ;

    float _axisCutValueX = 0;
    float _axisCutValueY = 0;
    float _axisCutValueZ = 0;

    int axisIndex;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
