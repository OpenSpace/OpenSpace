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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBELABELSCOMPONENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBELABELSCOMPONENT___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/fadeable.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/glm.h>
#include <ghoul/font/font.h>
#include <filesystem>
#include <memory>

namespace ghoul {
    namespace opengl { class ProgramObject; }
    class Dictionary;
} // namespace ghoul

namespace openspace {

struct Documentation;
class RenderableGlobe;
struct RenderData;

class GlobeLabelsComponent : public PropertyOwner, public Fadeable {
public:
    GlobeLabelsComponent();
    ~GlobeLabelsComponent() override = default;

    void initialize(const ghoul::Dictionary& dictionary, RenderableGlobe* globe);

    void initializeFonts();

    static openspace::Documentation Documentation();

    void draw(const RenderData& data);

private:
    bool loadLabelsData(const std::filesystem::path& file);
    bool readLabelsFile(const std::filesystem::path& file);
    bool loadCachedFile(const std::filesystem::path& file);
    bool saveCachedFile(const std::filesystem::path& file) const;
    void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        float distToCamera, float fadeInVariable);

    // Labels Structures
    struct LabelEntry {
        char feature[256];
        float diameter = 0.f;
        float latitude = 0.f;
        float longitude = 0.f;
        glm::vec3 geoPosition = glm::vec3(0.f);
    };

    struct Labels {
        std::string filename;
        std::vector<LabelEntry> labelsArray;
    };

    BoolProperty _enabled;
    Vec3Property _color;
    FloatProperty _fontSize;
    FloatProperty _size;
    IVec2Property _minMaxSize;
    FloatProperty _heightOffset;
    Vec2Property _fadeDistances;
    BoolProperty _fadeInEnabled;
    BoolProperty _fadeOutEnabled;
    BoolProperty _disableCulling;
    FloatProperty _distanceEPS;
    OptionProperty _alignmentOption;

    Labels _labels;

    // Font
    std::shared_ptr<ghoul::fontrendering::Font> _font;

    // Globe
    RenderableGlobe* _globe = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBELABELSCOMPONENT___H__
