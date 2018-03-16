/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__
#define __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class OctreeManager;

namespace documentation { struct Documentation; }

class RenderableGaiaStars : public Renderable {
public:
    explicit RenderableGaiaStars(const ghoul::Dictionary& dictionary);
    ~RenderableGaiaStars();

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    enum ColumnOption {
        Static = 0,
        Motion = 1,
        Color = 2
    };
    
    std::vector<float> sliceStarValues(ColumnOption option, std::vector<float> starValues);
    void createDataSlice(ColumnOption option);
    bool readFitsFile(ColumnOption option);
    float convertMasPerYearToMeterPerSecond(float masPerYear, float parallax);

    properties::StringProperty _fitsFilePath;
    std::unique_ptr<ghoul::filesystem::File> _fitsFile;
    bool _dataIsDirty;

    properties::StringProperty _pointSpreadFunctionTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _pointSpreadFunctionTexture;
    std::unique_ptr<ghoul::filesystem::File> _pointSpreadFunctionFile;
    bool _pointSpreadFunctionTextureIsDirty;

    properties::StringProperty _colorTexturePath;
    std::unique_ptr<ghoul::opengl::Texture> _colorTexture;
    std::unique_ptr<ghoul::filesystem::File> _colorTextureFile;
    bool _colorTextureIsDirty;

    properties::FloatProperty _magnitudeExponent;
    properties::FloatProperty _sharpness;
    properties::FloatProperty _billboardSize;
    properties::FloatProperty _closeUpBoostDist;

    properties::IntProperty _firstRow;
    properties::IntProperty _lastRow;
    properties::StringListProperty _columnNamesList;
    std::vector<std::string> _columnNames;
    properties::BoolProperty _filePreprocessed;
    properties::OptionProperty _columnOption;
    properties::IntProperty _nRenderedStars;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(model, view, viewScaling, projection, magnitudeExponent, sharpness, 
        billboardSize, screenSize, psfTexture, time, colorTexture, columnOption, 
        closeUpBoostDist) _uniformCache;

    std::shared_ptr<OctreeManager> _octreeManager;

    std::vector<float> _slicedData;
    std::vector<float> _fullData;
    size_t _nValuesPerStar;
    size_t _nValuesInSlice;
    size_t _maxStarsSize;

    GLuint _vao;
    GLuint _vbo;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___RENDERABLEGAIASTARS___H__
