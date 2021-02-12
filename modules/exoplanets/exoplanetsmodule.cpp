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
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>

#include "exoplanetsmodule_lua.inl"

namespace {
    constexpr const openspace::properties::Property::PropertyInfo DataFolderInfo = {
        "DataFolder",
        "Data Folder",
        "The path to the folder containing the exoplanets data and lookup table"
    };

    constexpr const openspace::properties::Property::PropertyInfo StarTextureInfo = {
        "StarTexture",
        "Star Texture",
        "The path to a grayscale image that is used for the host star surfaces"
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

    constexpr const char ExoplanetsDataFileName[] = "exoplanets_data.bin";
    constexpr const char LookupTableFileName[] = "lookup.txt";

    struct [[codegen::Dictionary(ExoplanetsModule)]] Parameters {
        // [[codegen::verbatim(DataFolderInfo.description)]]
        std::optional<std::string> dataFolder;

       // [[codegen::verbatim(StarTextureInfo.description)]]
       std::optional<std::string> starTexture;

       // [[codegen::verbatim(NoDataTextureInfo.description)]]
       std::optional<std::string> noDataTexture;

       // [[codegen::verbatim(OrbitDiscTextureInfo.description)]]
       std::optional<std::string> orbitDiscTexture;

       // [[codegen::verbatim(HabitableZoneTextureInfo.description)]]
       std::optional<std::string> habitableZoneTexture;

       // [[codegen::verbatim(ShowComparisonCircleInfo.description)]]
       std::optional<bool> showComparisonCircle;

       // [[codegen::verbatim(ShowHabitableZoneInfo.description)]]
       std::optional<bool> showHabitableZone;

       // [[codegen::verbatim(UseOptimisticZoneInfo.description)]]
       std::optional<bool> useOptimisticZone;
    };
#include "exoplanetsmodule_codegen.cpp"
} // namespace

namespace openspace {

using namespace exoplanets;

ExoplanetsModule::ExoplanetsModule()
    : OpenSpaceModule(Name)
    , _exoplanetsDataFolder(DataFolderInfo)
    , _starTexturePath(StarTextureInfo)
    , _noDataTexturePath(NoDataTextureInfo)
    , _orbitDiscTexturePath(OrbitDiscTextureInfo)
    , _habitableZoneTexturePath(HabitableZoneTextureInfo)
    , _showComparisonCircle(ShowComparisonCircleInfo, false)
    , _showHabitableZone(ShowHabitableZoneInfo, true)
    , _useOptimisticZone(UseOptimisticZoneInfo, true)
{
    _exoplanetsDataFolder.setReadOnly(true);

    addProperty(_exoplanetsDataFolder);
    addProperty(_starTexturePath);
    addProperty(_noDataTexturePath);
    addProperty(_orbitDiscTexturePath);
    addProperty(_habitableZoneTexturePath);
    addProperty(_showComparisonCircle);
    addProperty(_showHabitableZone);
    addProperty(_useOptimisticZone);
}

std::string ExoplanetsModule::exoplanetsDataPath() const {
    return absPath(
        fmt::format("{}/{}", _exoplanetsDataFolder, ExoplanetsDataFileName)
    );
};

std::string ExoplanetsModule::lookUpTablePath() const {
    return absPath(
        fmt::format("{}/{}", _exoplanetsDataFolder, LookupTableFileName)
    );
};

std::string ExoplanetsModule::starTexturePath() const {
    return _starTexturePath;
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
    _exoplanetsDataFolder = p.dataFolder.value_or(_exoplanetsDataFolder);
    _starTexturePath = p.starTexture.value_or(_starTexturePath);
    _noDataTexturePath = p.noDataTexture.value_or(_noDataTexturePath);
    _orbitDiscTexturePath = p.orbitDiscTexture.value_or(_orbitDiscTexturePath);
    _habitableZoneTexturePath = p.habitableZoneTexture.value_or(_habitableZoneTexturePath);

    _showComparisonCircle = p.showComparisonCircle.value_or(_showComparisonCircle);
    _showHabitableZone = p.showHabitableZone.value_or(_showHabitableZone);
    _useOptimisticZone = p.useOptimisticZone.value_or(_useOptimisticZone);

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
