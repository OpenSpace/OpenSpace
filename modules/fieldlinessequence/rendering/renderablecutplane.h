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

    bool isReady() const override;
    
    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();
    
protected:
    virtual void bindTexture() override;
  //  virtual void unbindTexture() override;

    void createPlane();

    ghoul::opengl::ProgramObject* _shader = nullptr;
    GLuint _vertexColorBuffer = 0;
    GLuint _vertexPositionBuffer = 0;
    GLuint vaoHandle;
    
private:
    void loadTexture();
    void updateVertexColorBuffer();
    std::unique_ptr<ghoul::opengl::Texture> createFloatTexture(const std::vector<std::vector<float>>& data);
    GameraVolumeSlicer _slice;
    properties::StringProperty _filePath;
    std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
    std::unique_ptr<TransferFunction> _transferFunction;
    glm::vec2 _textureDimensions = glm::vec2(0.f);
    std::unique_ptr<ghoul::filesystem::File> _sourceFile;
    std::string _axis;

    float _cutValue;
    std::string _dataProperty;
    // Used to save property for later initialization
    int _dataPropertyIndex;
    // Color table/transfer function for "By Quantity" coloring
    properties::StringProperty _colorTablePath;
    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::string> _colorTablePaths;
    // Values represents min & max values represented in the color table
    std::vector<glm::vec2> _colorTableRanges;

    //The axis dimensions of the axis we're not slicing on
    std::vector<std::vector<float>> _axisDim;
    int _axis1;
    int _axis2;
    // OpenGL Vertex Buffer Object containing the extraQuantity values used for coloring
    // the lines
    bool _planeIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
