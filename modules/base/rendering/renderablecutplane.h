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
    void createPlane();

private:
    void loadTexture();

    properties::StringProperty _filePath;
    ghoul::opengl::Texture* _texture = nullptr;
    glm::vec2 _textureDimensions = glm::vec2(0.f);
    std::unique_ptr<ghoul::filesystem::File> _sourceFile;
    std::string _axis;
    float _cutValue;
    std::string _colorQuantity;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
