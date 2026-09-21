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

#include <modules/exoplanetsexperttool/glyphhandler.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>
#include <modules/exoplanetsexperttool/rendering/renderablehostcloud.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <chrono>

namespace {
    using namespace openspace;

    // @TODO this could be a templated helper function for lists. Used a lot
    std::string formatIndicesList(const std::vector<size_t>& indices) {
        std::string result;
        for (size_t i : indices) {
            result += std::to_string(i) + ',';
        }
        if (!result.empty()) {
            result.pop_back();
        }
        return result;
    }

    void setStarGlyphsEnabled(bool enabled) {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Enabled', {})",
            exoplanets::GlyphHandler::HostCloudIdentifier, enabled
        ));
    }

    void setPlanetGlyphsEnabled(bool enabled) {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Enabled', {})",
            exoplanets::GlyphHandler::GlyphCloudIdentifier, enabled
        ));
    }

    void setBoolProperty(std::string_view identifier,
                         std::string_view property, bool value)
    {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            std::format("Scene.{}.Renderable.{}", identifier, property), value
        ));
    };
}

namespace openspace::exoplanets {

GlyphHandler::GlyphHandler(DataViewer& dataViewer)
    : _dataViewer(dataViewer)
{}

void GlyphHandler::initializeRenderables() {
    using namespace std::string_literals;

    _dataViewer.updateGlyphRenderData();

    // Glyphs
    ghoul::Dictionary gui;
    gui.setValue("Name", "Glyphs - Planets"s);
    gui.setValue("Path", "/ExoplanetExplorer"s);

    ghoul::Dictionary renderable;
    renderable.setValue("Type", "RenderableExoplanetGlyphCloud"s);
    renderable.setValue("Scale", static_cast<double>(DefaultGlyphScale));
    renderable.setValue("UseFixedWidth", false);
    renderable.setValue("RenderBinMode", "PreDeferredTransparent"s);

    ghoul::Dictionary node;
    node.setValue("Identifier", std::string(GlyphCloudIdentifier));
    node.setValue("Renderable", renderable);
    node.setValue("GUI", gui);

    global::scriptEngine->queueScript(
        std::format("openspace.addSceneGraphNode({})", ghoul::formatLua(node))
    );

    // Stars
    ghoul::Dictionary hostsGui;
    hostsGui.setValue("Name", "Glyphs - Stars"s);
    hostsGui.setValue("Path", "/ExoplanetExplorer"s);

    ghoul::Dictionary hostsRenderable;
    hostsRenderable.setValue("Type", "RenderableHostCloud"s);
    hostsRenderable.setValue("Scale", static_cast<double>(DefaultGlyphScale));
    hostsRenderable.setValue("RenderBinMode", "PreDeferredTransparent"s);
    hostsRenderable.setValue("Enabled", false);

    ghoul::Dictionary hostsNode;
    hostsNode.setValue("Identifier", std::string(HostCloudIdentifier));
    hostsNode.setValue("Renderable", hostsRenderable);
    hostsNode.setValue("GUI", hostsGui);

    global::scriptEngine->queueScript(
        std::format("openspace.addSceneGraphNode({})", ghoul::formatLua(hostsNode))
    );
}

void GlyphHandler::updateSelectionInRenderable(const std::vector<size_t>& selection) {
    const std::string indices = formatIndicesList(selection);

    global::scriptEngine->queueScript({
        .code = std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Selection', {{ {} }})",
            GlyphCloudIdentifier, indices
        ),
        .addToLog = ScriptEngine::Script::ShouldBeLogged::No
    });

    global::scriptEngine->queueScript({
        .code = std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Selection', {{ {} }})",
            HostCloudIdentifier, indices
        ),
        .addToLog = ScriptEngine::Script::ShouldBeLogged::No
    });
}

int GlyphHandler::getHoveredPlanetIndex() const {
    // Start by checking the glyph cloud
    SceneGraphNode* n = sceneGraphNode(GlyphCloudIdentifier);
    if (n) {
        RenderableExoplanetGlyphCloud* cloud =
            dynamic_cast<RenderableExoplanetGlyphCloud*>(n->renderable());
        if (cloud && cloud->isEnabled()) {
            int index = cloud->hoveredIndex();
            if (index != -1) {
                return index;
            }
        }
    }

    // Then, the host renderable
    SceneGraphNode* n2 = sceneGraphNode(HostCloudIdentifier);
    if (n2) {
        RenderableHostCloud* cloud = dynamic_cast<RenderableHostCloud*>(n2->renderable());
        if (cloud && cloud->isEnabled()) {
            int index = cloud->hoveredIndex();
            if (index != -1) {
                return index;
            }
        }
    }

    return -1;
}

void GlyphHandler::setGlyphScale(float scale) {
    global::scriptEngine->queueScript(std::format(
        "openspace.setPropertyValueSingle('Scene.{}.Renderable.Scale', {})",
        GlyphCloudIdentifier, scale
    ));

    global::scriptEngine->queueScript(std::format(
        "openspace.setPropertyValueSingle('Scene.{}.Renderable.Scale', {})",
        HostCloudIdentifier, scale
    ));
}

void GlyphHandler::setGlyphMode(GlyphMode mode) {
    if (_mode == mode) {
        return;
    }

    auto setPlanetMode = [](std::string_view mode) {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.GlyphMode', '{}')",
            GlyphCloudIdentifier, mode
        ));
    };

    switch (mode) {
        case GlyphMode::Rings:
            setStarGlyphsEnabled(false);
            setPlanetGlyphsEnabled(true);
            setPlanetMode("Rings");
            break;
        case GlyphMode::Inclination:
            setStarGlyphsEnabled(false);
            setPlanetGlyphsEnabled(true);
            setPlanetMode("Inclination");
            break;
        case GlyphMode::Star:
            setStarGlyphsEnabled(true);
            setBoolProperty(HostCloudIdentifier, "UseSecondMappedColor", false);
            setPlanetGlyphsEnabled(false);
            break;
        case GlyphMode::InclinationAndStar:
            setStarGlyphsEnabled(true);
            setBoolProperty(HostCloudIdentifier, "UseSecondMappedColor", true);
            setPlanetGlyphsEnabled(true);

            // TODO: Warn about having to add an extra color mapping for the star,
            // or provide a reasonable default color
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    _mode = mode;
}

void GlyphHandler::renderModeSpecificSettings() {
    // OBS! These should match the default settings for the SGNs

    // RenderableExoplanetGlyphCloud
    static bool useFixedWidth = false;
    static bool starEnabled = false;

    // RenderableHostCloud
    static bool useAdditiveBlending = true;
    static bool shouldBlurPoints = true;

    if (_mode == GlyphMode::Rings) {
        if (ImGui::Checkbox("Use fixed ring width", &useFixedWidth)) {
            setBoolProperty(GlyphCloudIdentifier, "UseFixedWidth", useFixedWidth);
        }
    }
    else if (_mode == GlyphMode::Inclination) {
        if (ImGui::Checkbox("Show lines from star", &starEnabled)) {
            setBoolProperty(GlyphCloudIdentifier, "StarGlyph.Enabled", starEnabled);
        }
    }
    else if (_mode == GlyphMode::Star) {
        if (ImGui::Checkbox("Use additive blending", &useAdditiveBlending)) {
            setBoolProperty(HostCloudIdentifier, "UseAdditiveBlending", useAdditiveBlending);
        }
        if (ImGui::Checkbox("Should blur points", &shouldBlurPoints)) {
            setBoolProperty(HostCloudIdentifier, "ShouldBlurPoints", shouldBlurPoints);
        }
    }
    else if (_mode == GlyphMode::InclinationAndStar) {
        if (ImGui::Checkbox("Show lines from star", &starEnabled)) {
            setBoolProperty(GlyphCloudIdentifier, "StarGlyph.Enabled", starEnabled);
        }
        if (ImGui::Checkbox("Use additive blending (star)", &useAdditiveBlending)) {
            setBoolProperty(HostCloudIdentifier, "UseAdditiveBlending", useAdditiveBlending);
        }
        if (ImGui::Checkbox("Should blur points (star)", &shouldBlurPoints)) {
            setBoolProperty(HostCloudIdentifier, "ShouldBlurPoints", shouldBlurPoints);
        }
    }
    else {
        throw ghoul::MissingCaseException();
    }
}

} // namespace openspace::exoplanets
