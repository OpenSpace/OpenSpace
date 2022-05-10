/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/optionproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <optional>

namespace openspace::documentation { struct Documentation; }

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class RenderablePointsCloud : public Renderable {
public:
    RenderablePointsCloud(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    void createDataSlice();
    void loadData();
    void loadColorMap();

    bool _hasPointData = false;
    bool _isDirty = true;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram = nullptr;
    UniformCache(color, opacity, size, modelMatrix, cameraUp,
        cameraViewProjectionMatrix, eyePosition, sizeOption) _uniformCache;

    properties::BoolProperty _isVisible;
    properties::FloatProperty _size;
    properties::Vec3Property _color;
    properties::OptionProperty _sizeOption;
    
    std::vector<float> _fullData;
    std::vector<float> _slicedData;
    std::unique_ptr<ghoul::opengl::Texture> _colorMapTexture;

    std::optional<std::string> _dataStorageKey = std::nullopt;
    std::optional<std::string> _identifier = std::nullopt;
    properties::BoolProperty _colorMapEnabled;
    properties::BoolProperty _loadNewColorMap;

    int _nValuesPerPoint = 0;

    GLuint _vertexArrayObjectID = 0;
    GLuint _vertexBufferObjectID = 0;

    enum SizeOption {
        Uniform = 0,
        NonUniform = 1
    };
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__
