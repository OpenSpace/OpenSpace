/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___COLORMAPCOMPONENT___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___COLORMAPCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <modules/space/speckloader.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * TODO:
 */
class ColorMapComponent : public properties::PropertyOwner {
public:
    ColorMapComponent();
    explicit ColorMapComponent(const ghoul::Dictionary& dictionary);
    ~ColorMapComponent() override = default;

    ghoul::opengl::Texture* texture() const;

    /**
     * Initialize the color map information (ranges, etc.) based on the input dataset
     *
     * \param dataset the *loaded* input dataset
     */
    void initialize(const speck::Dataset& dataset);

    /**
     * Initialize a 1D texture based on the entries in the color map file
     */
    void initializeTexture();

    static documentation::Documentation Documentation();

    glm::vec4 colorFromColorMap(float valueToColorFrom) const;

    properties::BoolProperty enabled;
    properties::OptionProperty dataColumn;
    properties::StringProperty colorMapFile;
    properties::Vec2Property valueRange;
    properties::TriggerProperty setRangeFromData;

    properties::BoolProperty hideOutsideRange;
    properties::BoolProperty useNanColor;
    properties::Vec4Property nanColor;

private:
    // One item per color parameter option
    std::vector<glm::vec2> _colorRangeData;

    std::unique_ptr<ghoul::opengl::Texture> _texture;

    speck::ColorMap _colorMap;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___COLORMAPCOMPONENT___H__
