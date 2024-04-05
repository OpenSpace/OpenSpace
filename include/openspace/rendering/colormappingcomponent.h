/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___COLORMAPPINGCOMPONENT___H__
#define __OPENSPACE_CORE___COLORMAPPINGCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/data/dataloader.h>
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
 * This is a component that can be used to consistently hold parameters and properties
 * for controlling color mapping in different types of renderables. This includes things
 * like the color map file itself (converted to a texture), colors to use for missing
 * values and the available data columns and value ranges.
 *
 * @TODO Also provide a small shader snippet that can be included in fragment shaders
 * that use this color mapping. As well as a set of uniforms? Now every
 * renderable needs to handle this separately.  (emmbr, 2023-10-13)
 */
class ColorMappingComponent : public properties::PropertyOwner {
public:
    ColorMappingComponent();
    explicit ColorMappingComponent(const ghoul::Dictionary& dictionary);
    ~ColorMappingComponent() override = default;

    ghoul::opengl::Texture* texture() const;

    /**
     * Initialize the color map information (ranges, etc.) based on the input dataset.
     *
     * \param dataset The *loaded* input dataset
     */
    void initialize(const dataloader::Dataset& dataset);

    /**
     * Initialize a 1D texture based on the entries in the color map file.
     */
    void initializeTexture();

    void update(const dataloader::Dataset& dataset);

    static documentation::Documentation Documentation();

    glm::vec4 colorFromColorMap(float valueToColorFrom) const;

    properties::BoolProperty enabled;
    properties::BoolProperty invert;
    properties::OptionProperty dataColumn;
    properties::StringProperty colorMapFile;
    properties::Vec2Property valueRange;
    properties::TriggerProperty setRangeFromData;

    properties::BoolProperty hideOutsideRange;
    properties::BoolProperty useNanColor;
    properties::Vec4Property nanColor;

    properties::BoolProperty useAboveRangeColor;
    properties::Vec4Property aboveRangeColor;

    properties::BoolProperty useBelowRangeColor;
    properties::Vec4Property belowRangeColor;

private:
    /**
     * Fill parameter options list and range data based on the dataset and provided
     * information.
     */
    void initializeParameterData(const dataloader::Dataset& dataset);

    // One item per color parameter option
    std::vector<glm::vec2> _colorRangeData;

    std::unique_ptr<ghoul::opengl::Texture> _texture;

    dataloader::ColorMap _colorMap;

    std::optional<std::string> _providedParameter;
    std::optional<glm::vec2> _providedRange;

    bool _hasNanColorInAsset = false;
    bool _hasBelowRangeColorInAsset = false;
    bool _hasAboveRangeColorInAsset = false;

    bool _colorMapFileIsDirty = true;
    bool _colorMapTextureIsDirty = true;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___COLORMAPPINGCOMPONENT___H__
