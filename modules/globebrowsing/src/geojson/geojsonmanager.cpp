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

#include <modules/globebrowsing/src/geojson/geojsonmanager.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>


namespace {
    constexpr std::string_view _loggerCat = "GeoJsonManager";
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GeoJsonManager::Documentation() {
    using namespace documentation;
    return {
        "GeoJsonManager",
        "globebrowsing_geojsonmanager",
        "",
        {}
    };
}

// TODO: Gui name and description
GeoJsonManager::GeoJsonManager() : properties::PropertyOwner({ "GeoJson" }) {}

void GeoJsonManager::initialize(RenderableGlobe* globe) {
    ghoul_assert(globe, "No globe provided");
    _parentGlobe = globe;
}

void GeoJsonManager::deinitializeGL() {
    for (const std::unique_ptr<GeoJsonComponent>& g : _geoJsonObjects) {
        g->deinitializeGL();
    }
}

bool GeoJsonManager::isReady() const {
    const bool isReady = std::all_of(
        std::begin(_geoJsonObjects),
        std::end(_geoJsonObjects),
        [](const std::unique_ptr<GeoJsonComponent>& g) {
            return g->isReady();
        }
    );
    return isReady;
}

void GeoJsonManager::addGeoJsonLayer(const ghoul::Dictionary& layerDict) {
    ZoneScoped;

    try {
        // Parse dictionary
        documentation::testSpecificationAndThrow(
            GeoJsonComponent::Documentation(),
            layerDict,
            "GeoJsonComponent"
        );

        const std::string identifier = layerDict.value<std::string>("Identifier");
        if (hasPropertySubOwner(identifier)) {
            LERROR("GeoJson layer with identifier '" + identifier + "' already exists");
            return;
        }

        // TODO: use owner instead of explicit parent?
        std::unique_ptr<GeoJsonComponent> geo =
            std::make_unique<GeoJsonComponent>(layerDict, *_parentGlobe);

        geo->initializeGL();

        GeoJsonComponent* ptr = geo.get();
        _geoJsonObjects.push_back(std::move(geo));
        addPropertySubOwner(ptr);
    }
    catch (const documentation::SpecificationError& e) {
        logError(e);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }
}

void GeoJsonManager::deleteLayer(const std::string& layerIdentifier) {
    ZoneScoped;

    for (auto it = _geoJsonObjects.begin(); it != _geoJsonObjects.end(); it++) {
        if (it->get()->identifier() == layerIdentifier) {
            LINFO("Deleting GeoJson layer: " + layerIdentifier);
            removePropertySubOwner(it->get());
            (*it)->deinitializeGL();
            _geoJsonObjects.erase(it);
            return;
        }
    }
    LERROR("Could not find GeoJson layer " + layerIdentifier);
}

void GeoJsonManager::update() {
    ZoneScoped;

    for (std::unique_ptr<GeoJsonComponent>& obj : _geoJsonObjects) {
        if (obj->enabled()) {
            obj->update();
        }
    }
}

void GeoJsonManager::render(const RenderData& data) {
    ZoneScoped;

    for (std::unique_ptr<GeoJsonComponent>& obj : _geoJsonObjects) {
        if (obj->enabled()) {
            obj->render(data);
        }
    }
}

} // namespace openspace::globebrowsing
