/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___LABELSCOMPONENT___H__
#define __OPENSPACE_MODULE_SPACE___LABELSCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <modules/space/speckloader.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/glm.h>
#include <filesystem>

namespace ghoul::fontrendering { class Font; }

namespace openspace {
    struct RenderData;

namespace documentation { struct Documentation; }

namespace speck {

class LabelsComponent : public properties::PropertyOwner {
public:
    explicit LabelsComponent(const ghoul::Dictionary& dictionary);
    ~LabelsComponent() override = default;

    speck::Labelset& labelSet();
    const speck::Labelset& labelSet() const;

    void initialize();

    void loadLabels();

    bool isReady() const;

    void render(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::vec3& orthoRight, const glm::vec3& orthoUp,
        float fadeInVariable = 1.f);

    static documentation::Documentation Documentation();

private:
    std::filesystem::path _labelFile;
    DistanceUnit _unit = DistanceUnit::Parsec;
    speck::Labelset _labelset;

    std::shared_ptr<ghoul::fontrendering::Font> _font = nullptr;

    // Properties
    properties::FloatProperty _opacity;
    properties::Vec3Property _color;
    properties::FloatProperty _size;
    properties::IVec2Property _minMaxSize;

    // DEBUG:
    properties::OptionProperty _renderOption;
};

} // namespace speck

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___LABELSCOMPONENT___H__
