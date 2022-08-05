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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATION___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATION___H__

#include <openspace/rendering/renderable.h>

#include <modules/space/speckloader.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <map>
#include <vector>

namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableConstellation : public Renderable {
public:
    virtual ~RenderableConstellation() override = default;

    virtual void initialize() override;
    virtual void initializeGL() override = 0;
    virtual void deinitialize() override = 0;
    virtual void deinitializeGL() override = 0;

    virtual bool isReady() const override = 0;

    virtual void render(const RenderData& data, RendererTasks& rendererTask) override;
    void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::vec3& orthoRight, const glm::vec3& orthoUp);
    virtual void update(const UpdateData& data) override = 0;

    static documentation::Documentation Documentation();

protected:
    explicit RenderableConstellation(const ghoul::Dictionary& dictionary);

    /**
     * Callback method that gets triggered when <code>_constellationSelection</code>
     * changes.
     */
    virtual void selectionPropertyHasChanged() = 0;

    // Map over the constellations names and theis abbreviations
    // key = abbreviations, value = full name
    std::map<std::string, std::string> _constellationNamesTranslation;

    // Linewidth for the constellation bounds
    properties::FloatProperty _lineWidth;

    /// The property that stores all indices of constellations that should be drawn
    properties::SelectionProperty _constellationSelection;

    bool _hasLabel = false;
    properties::BoolProperty _drawLabels;
    speck::Labelset _labelset;

private:
    std::vector<std::string> _assetSelectedMeshes;

    /**
     * Loads the file specified in _constellationNamesFilename that contains the mapping
     * between abbreviations and full names of constellations.
     *
     * \return <code>true</code> if the loading succeeded, <code>false</code> otherwise
     */
    void loadConstellationFile();

    /// Fills the <code>_constellationSelection</code> property with all constellations
    void fillSelectionProperty();

    /// The file containing constellation names and abbreviations
    properties::StringProperty _constellationNamesFilename;

    //Label text settings
    std::string _labelFile;
    properties::Vec3Property _textColor;
    bool _textColorIsDirty = true;
    properties::FloatProperty _textOpacity;
    properties::FloatProperty _textSize;
    properties::IVec2Property _textMinMaxSize;
    std::shared_ptr<ghoul::fontrendering::Font> _font = nullptr;
    DistanceUnit _labelUnit = DistanceUnit::Parsec;

    properties::OptionProperty _renderOption;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATION___H__
