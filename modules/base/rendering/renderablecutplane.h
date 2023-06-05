//
//  renderablecutplane.hpp
//  openspace-module-base
//
//  Created by Julia Johnstone on 2023-05-08.
//

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__

#include <modules/base/rendering/renderableplane.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class Texture; }

namespace openspace {

struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableCutPlane : public RenderablePlane {
public:
    RenderableCutPlane(const ghoul::Dictionary& dictionary);
//    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();
    
    void readHdfFile(std::string pathToHdf5File);
    void slicer(char axis, float value);
    void interpolator(float value);

protected:
//    virtual void bindTexture() override;
    void createPlane();

private:
//    void loadTexture();

    properties::StringProperty _filePath;
//    ghoul::opengl::Texture* _texture = nullptr;
    glm::vec2 _textureDimensions = glm::vec2(0.f);
    std::unique_ptr<ghoul::filesystem::File> _sourceFile;
    // two more vec2
    std::string _axis;
    float _value;
    
    
    std::vector<std::vector<std::vector<std::vector<float>>>> _volumeCoordinates;

    std::vector<std::string> _extraQuantatiesNames;
    std::vector<std::vector<std::vector<std::vector<float>>>> _extraQuantaties;

    std::vector<std::vector<std::vector<float>>> _slicedDataBP; // Slice fo data BEFORE position of slice
    std::vector<std::vector<std::vector<float>>> _slicedDataAP; // Slice fo data AFTER position of slice

    std::vector<std::vector<std::vector<float>>> _interpolatedData;
    

    bool _isLoadingLazily = false;
    bool _textureIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECUTPLANE___H__
