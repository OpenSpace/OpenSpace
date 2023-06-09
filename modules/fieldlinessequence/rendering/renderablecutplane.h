//
//  renderablecutplane.hpp
//  openspace-module-base
//
//  Created by Julia Johnstone on 2023-05-08.
//

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__

#include <modules/base/rendering/renderableplane.h>

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
    
//    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();
    
protected:
    virtual void bindTexture() override;
//    void createPlane();

private:
    void loadTexture();

    properties::StringProperty _filePath;
    ghoul::opengl::Texture* _texture = nullptr;
    glm::vec2 _textureDimensions = glm::vec2(0.f);
    std::unique_ptr<ghoul::filesystem::File> _sourceFile;
    std::string _axis;
    float _cutValue;
    std::string _colorQuantity;

    std::vector<std::vector<int>> _axisDim;
    int _axis1;
    int _axis2;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
