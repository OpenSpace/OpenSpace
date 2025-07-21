/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONSBASE___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONSBASE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/selectionproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/rendering/labelscomponent.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <map>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This is a base class for constellation lines and bounds.
 */
class RenderableConstellationsBase : public Renderable {
public:
    virtual ~RenderableConstellationsBase() override = default;

    virtual void initialize() override;
    virtual void initializeGL() override = 0;
    virtual void deinitializeGL() override = 0;

    virtual bool isReady() const override;

    virtual void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

protected:
    explicit RenderableConstellationsBase(const ghoul::Dictionary& dictionary);

    /**
     * Callback method that gets triggered when `_constellationSelection` changes.
     */
    virtual void selectionPropertyHasChanged() = 0;

    /**
     * Takes the given constellation `identifier` and returns the corresponding full name.
     */
    std::string constellationFullName(const std::string& identifier) const;

    /// Width for the rendered lines
    properties::FloatProperty _lineWidth;

    /// Property that stores all constellations chosen by the user to be drawn
    properties::SelectionProperty _selection;

    /// Temporary storage of which constellations should be rendered as stated in the
    /// asset file
    std::vector<std::string> _assetSelection;

    /// Labels
    bool _hasLabels = false;
    /// Everything related to the labels is handled by LabelsComponent
    std::unique_ptr<LabelsComponent> _labels;

private:
    /// Map over the constellations names and their abbreviations
    /// key = abbreviation, value = full name
    std::map<std::string, std::string> _namesTranslation;

    /**
     * Loads the file specified in `_constellationNamesFilename` that contains the mapping
     * between abbreviations and full names of constellations.
     */
    void loadConstellationFile();

    /**
     * Fills the `_constellationSelection` property with all constellations.
     */
    void fillSelectionProperty();

    /// The file containing constellation names and abbreviations
    properties::StringProperty _namesFilename;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONSBASE___H__
