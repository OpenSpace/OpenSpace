/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/exoplanets/exoplanetsmodule.h>

#include <modules/exoplanets/rendering/renderableorbitdisc.h>
#include <modules/exoplanets/tasks/exoplanetsdatapreparationtask.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <filesystem>

#include "exoplanetsmodule_lua.inl"

namespace {
    constexpr const openspace::properties::Property::PropertyInfo DataFolderInfo = {
        "DataFolder",
        "Data Folder",
        "The path to the folder containing the exoplanets data and lookup table"
    };

    constexpr const openspace::properties::Property::PropertyInfo BvColorMapInfo = {
        "BvColormap",
        "B-V Colormap",
        "The path to a cmap file that maps a B-V color index to an RGB color"
    };

    constexpr const openspace::properties::Property::PropertyInfo StarTextureInfo = {
        "StarTexture",
        "Star Texture",
        "The path to a grayscale image that is used for the host star surfaces"
    };

    constexpr const openspace::properties::Property::PropertyInfo StarGlareTextureInfo = {
        "StarGlareTexture",
        "Star Glare Texture",
        "The path to a grayscale image that is used for the glare effect of the "
        "host stars"
    };

    constexpr const openspace::properties::Property::PropertyInfo NoDataTextureInfo = {
        "NoDataTexture",
        "No Data Star Texture",
        "A path to a texture that is used to represent that there is missing data about "
        "the star. For example no color information"
    };

    constexpr const openspace::properties::Property::PropertyInfo OrbitDiscTextureInfo =
    {
        "OrbitDiscTexture",
        "Orbit Disc Texture",
        "A path to a 1-dimensional image used as a transfer function for the "
        "exoplanets' orbit uncertainty disc"
    };

    constexpr const openspace::properties::Property::PropertyInfo
        HabitableZoneTextureInfo =
    {
        "HabitableZoneTexture",
        "Habitable Zone Texture",
        "A path to a 1-dimensional image used as a transfer function for the "
        "habitable zone disc"
    };

    constexpr const openspace::properties::Property::PropertyInfo
        ShowComparisonCircleInfo =
    {
        "ShowComparisonCircle",
        "Show Comparison Circle",
        "If true, the 1 AU size comparison circle is enabled per default when an "
        "exoplanet system is created"
    };

    constexpr const openspace::properties::Property::PropertyInfo
        ShowHabitableZoneInfo =
    {
        "ShowHabitableZone",
        "Show Habitable Zone",
        "If true, the habitable zone disc is enabled per default when an "
        "exoplanet system is created"
    };

    constexpr const openspace::properties::Property::PropertyInfo UseOptimisticZoneInfo =
    {
        "UseOptimisticZone",
        "Use Optimistic Zone Boundaries",
        "If true, the habitable zone is computed with optimistic boundaries per default "
        "when an exoplanet system is created"
    };

    constexpr const openspace::properties::Property::PropertyInfo
        HabitableZoneOpacityInfo =
    {
        "HabitableZoneOpacity",
        "Habitable Zone Opacity",
        "The opacity value used for the habitable zone renderable for a created "
        "exoplanet system"
    };

    constexpr const char ExoplanetsDataFileName[] = "exoplanets_data.bin";
    constexpr const char LookupTableFileName[] = "lookup.txt";

    struct [[codegen::Dictionary(ExoplanetsModule)]] Parameters {
        // [[codegen::verbatim(DataFolderInfo.description)]]
        std::optional<std::filesystem::path> dataFolder [[codegen::directory()]];

        // [[codegen::verbatim(BvColorMapInfo.description)]]
        std::optional<std::filesystem::path> bvColormap;

       // [[codegen::verbatim(StarTextureInfo.description)]]
       std::optional<std::filesystem::path> starTexture;

       // [[codegen::verbatim(StarGlareTextureInfo.description)]]
       std::optional<std::filesystem::path> starGlareTexture;

       // [[codegen::verbatim(NoDataTextureInfo.description)]]
       std::optional<std::filesystem::path> noDataTexture;

       // [[codegen::verbatim(OrbitDiscTextureInfo.description)]]
       std::optional<std::filesystem::path> orbitDiscTexture;

       // [[codegen::verbatim(HabitableZoneTextureInfo.description)]]
       std::optional<std::filesystem::path> habitableZoneTexture;

       // [[codegen::verbatim(ShowComparisonCircleInfo.description)]]
       std::optional<bool> showComparisonCircle;

       // [[codegen::verbatim(ShowHabitableZoneInfo.description)]]
       std::optional<bool> showHabitableZone;

       // [[codegen::verbatim(UseOptimisticZoneInfo.description)]]
       std::optional<bool> useOptimisticZone;

       // [[codegen::verbatim(HabitableZoneOpacityInfo.description)]]
       std::optional<float> habitableZoneOpacity [[codegen::inrange(0, 1)]];
    };
#include "exoplanetsmodule_codegen.cpp"
} // namespace

namespace openspace {

using namespace exoplanets;

ExoplanetsModule::ExoplanetsModule()
    : OpenSpaceModule(Name)
    , _exoplanetsDataFolder(DataFolderInfo)
    , _bvColorMapPath(BvColorMapInfo)
    , _starTexturePath(StarTextureInfo)
    , _starGlareTexturePath(StarGlareTextureInfo)
    , _noDataTexturePath(NoDataTextureInfo)
    , _orbitDiscTexturePath(OrbitDiscTextureInfo)
    , _habitableZoneTexturePath(HabitableZoneTextureInfo)
    , _showComparisonCircle(ShowComparisonCircleInfo, false)
    , _showHabitableZone(ShowHabitableZoneInfo, true)
    , _useOptimisticZone(UseOptimisticZoneInfo, true)
    , _habitableZoneOpacity(HabitableZoneOpacityInfo, 0.1f, 0.0f, 1.0f)
{
    _exoplanetsDataFolder.setReadOnly(true);

    addProperty(_exoplanetsDataFolder);
    addProperty(_bvColorMapPath);
    addProperty(_starTexturePath);
    addProperty(_starGlareTexturePath);
    addProperty(_noDataTexturePath);
    addProperty(_orbitDiscTexturePath);
    addProperty(_habitableZoneTexturePath);

    addProperty(_showComparisonCircle);
    addProperty(_showHabitableZone);
    addProperty(_useOptimisticZone);

    addProperty(_habitableZoneOpacity);
}

std::string ExoplanetsModule::exoplanetsDataPath() const {
    return absPath(
        fmt::format("{}/{}", _exoplanetsDataFolder.value(), ExoplanetsDataFileName)
    ).string();
}

std::string ExoplanetsModule::lookUpTablePath() const {
    return absPath(
        fmt::format("{}/{}", _exoplanetsDataFolder, LookupTableFileName)
    ).string();
}

std::string ExoplanetsModule::bvColormapPath() const {
    return _bvColorMapPath;
}

std::string ExoplanetsModule::starTexturePath() const {
    return _starTexturePath;
}

std::string ExoplanetsModule::starGlareTexturePath() const {
    return _starGlareTexturePath;
}

std::string ExoplanetsModule::noDataTexturePath() const {
    return _noDataTexturePath;
}

std::string ExoplanetsModule::orbitDiscTexturePath() const {
    return _orbitDiscTexturePath;
}

std::string ExoplanetsModule::habitableZoneTexturePath() const {
    return _habitableZoneTexturePath;
}

bool ExoplanetsModule::showComparisonCircle() const {
    return _showComparisonCircle;
}

bool ExoplanetsModule::showHabitableZone() const {
    return _showHabitableZone;
}

bool ExoplanetsModule::useOptimisticZone() const {
    return _useOptimisticZone;
}

float ExoplanetsModule::habitableZoneOpacity() const {
    return _habitableZoneOpacity;
}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addExoplanetSystem",
            &exoplanets::luascriptfunctions::addExoplanetSystem,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "removeExoplanetSystem",
            &exoplanets::luascriptfunctions::removeExoplanetSystem,
            {},
            "string",
            "Removes the nodes of the specified exoplanet system from the scene graph"
        },
        {
            "listAvailableExoplanetSystems",
            &exoplanets::luascriptfunctions::listAvailableExoplanetSystems,
            {},
            "",
            "Prints a list with the names of all exoplanet systems that can be added to "
            "the scene graph to the OpenSpace Log"
        },
        {
            "getListOfExoplanets",
            &exoplanets::luascriptfunctions::getListOfExoplanets,
            {},
            "",
            "Gets a list with the names of all exoplanet systems"
        }
    };

    return res;
}

void ExoplanetsModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    if (p.dataFolder.has_value()) {
        _exoplanetsDataFolder = p.dataFolder.value().string();
    }

    if (p.bvColormap.has_value()) {
        _bvColorMapPath = p.bvColormap.value().string();
    }

    if (p.starTexture.has_value()) {
        _starTexturePath = p.starTexture.value().string();
    }

    if (p.starGlareTexture.has_value()) {
        _starGlareTexturePath = p.starGlareTexture.value().string();
    }

    if (p.noDataTexture.has_value()) {
        _noDataTexturePath = p.noDataTexture.value().string();
    }

    if (p.orbitDiscTexture.has_value()) {
        _orbitDiscTexturePath = p.orbitDiscTexture.value().string();
    }

    if (p.habitableZoneTexture.has_value()) {
        _habitableZoneTexturePath = p.habitableZoneTexture.value().string();
    }

    _showComparisonCircle = p.showComparisonCircle.value_or(_showComparisonCircle);
    _showHabitableZone = p.showHabitableZone.value_or(_showHabitableZone);
    _useOptimisticZone = p.useOptimisticZone.value_or(_useOptimisticZone);

    _habitableZoneOpacity = p.habitableZoneOpacity.value_or(_habitableZoneOpacity);

    auto fTask = FactoryManager::ref().factory<Task>();
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ExoplanetsDataPreparationTask>("ExoplanetsDataPreparationTask");
    fRenderable->registerClass<RenderableOrbitDisc>("RenderableOrbitDisc");
}

std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}

} // namespace openspace
