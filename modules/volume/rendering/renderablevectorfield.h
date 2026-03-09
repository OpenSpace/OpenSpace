/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__
#define __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__

#include <openspace/rendering/renderable.h>

#include <modules/volume/rawvolume.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct Documentation;

class RenderableVectorField : public Renderable {
public:
   explicit RenderableVectorField(const ghoul::Dictionary& dictionary);

   ~RenderableVectorField() override = default;

   void initialize() override;
   void initializeGL() override;
   void deinitializeGL() override;

   bool isReady() const override;

   void render(const RenderData& data, RendererTasks& renderTask) override;
   void update(const UpdateData& data) override;

   static openspace::Documentation Documentation();

private:
    enum class Mode {
        Volume,
        Sparse
    };

    struct VelocityData {
        float vx;
        float vy;
        float vz;
    };

    struct ArrowInstance {
        glm::vec3 position;
        glm::vec3 direction;
        float magnitude;
    };

    void applyLuaFilter();
    void computeVolumeFieldLines();
    void computeSparseFieldLines();
    void loadVolumeData(const std::filesystem::path& path);
    void loadCSVData(const std::filesystem::path& path);

    Mode _mode = Mode::Volume;

    struct {
        std::shared_ptr<RawVolume<VelocityData>> volumeData;
        glm::vec3 minDomain;
        glm::vec3 maxDomain;
        glm::uvec3 dimensions;
    } _volume;

    struct CsvData {
        glm::vec3 position;
        glm::vec3 velocity;
    };

    struct {
        std::vector<CsvData> data;
        std::string xColumnName;
        std::string yColumnName;
        std::string zColumnName;
        std::string vxColumnName;
        std::string vyColumnName;
        std::string vzColumnName;
    } _sparse;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(
        modelViewProjection, arrowScale, colorMode, magDomain, colorTexture, opacity,
        fixedColor
    ) _uniformCache;

    struct ColorSettings : PropertyOwner {
        explicit ColorSettings(const ghoul::Dictionary& dictionary);
        OptionProperty colorModeOption;
        StringProperty colorMap;
        Vec2Property colorMagnitudeDomain;
        Vec4Property fixedColor;
        bool shouldComputeMagnitudeRange = true;
    };
    ColorSettings _colorSettings;

    IntProperty _stride;
    FloatProperty _vectorFieldScale;
    FloatProperty _lineWidth;

    BoolProperty _filterByLua;
    StringProperty _luaScriptFile;
    std::unique_ptr<ghoul::filesystem::File> _luaScriptFileHandle;

    ghoul::lua::LuaState _state;

    std::filesystem::path _sourceFile;
    std::vector<ArrowInstance> _instances;

    GLuint _vao = 0;
    GLuint _vectorFieldVbo = 0;
    std::unique_ptr<ghoul::opengl::Texture> _colorTexture;

    bool _vectorFieldIsDirty = true;
    bool _textureIsDirty = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__
