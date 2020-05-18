/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLELABELS___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLELABELS___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

struct LinePoint;

class RenderableLabels : public Renderable {
public:
    RenderableLabels(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

    void setLabelText(const std::string & newText);

protected:
    properties::OptionProperty _blendMode;

    float unit(int unit) const;

    std::string toString(int unit) const;

    // Data may require some type of transformation prior the spice transformation being
    // applied.
    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    enum Unit {
        Meter = 0,
        Kilometer,
        Megameter,
        Gigameter,
        AU,
        Terameter,
        Petameter,
        Parsec,
        Kiloparsec,
        Megaparsec,
        Gigaparsec,
        GigalightYears
    };

private:
    void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);

    float changedPerlinSmoothStepFunc(float x, float startX, float endX) const;

    float linearSmoothStepFunc(float x, float startX, float endX, float sUnit,
        float eUnit) const;

    properties::Vec4Property _labelColor;
    properties::FloatProperty _labelSize;
    properties::FloatProperty _fontSize;
    properties::FloatProperty _labelMinSize;
    properties::FloatProperty _labelMaxSize;
    properties::BoolProperty _pixelSizeControl;
    properties::BoolProperty _enableFadingEffect;
    properties::StringProperty _labelText;
    properties::FloatProperty _fadeStartDistance;
    properties::FloatProperty _fadeEndDistance;
    properties::FloatProperty _fadeStartSpeed;
    properties::FloatProperty _fadeEndSpeed;

    properties::OptionProperty _labelOrientationOption;
    properties::OptionProperty _fadeStartUnitOption;
    properties::OptionProperty _fadeEndUnitOption;

    std::shared_ptr<ghoul::fontrendering::Font> _font;

    std::string _speckFile;
    std::string _colorMapFile;
    std::string _labelFile;
    std::string _colorOptionString;
    std::string _datavarSizeOptionString;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLELABELS___H__
