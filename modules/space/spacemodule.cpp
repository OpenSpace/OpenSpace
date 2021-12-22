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

#include <modules/space/spacemodule.h>

#include <modules/space/rendering/renderableconstellationbounds.h>
#include <modules/space/rendering/renderablefluxnodes.h>
#include <modules/space/rendering/renderablehabitablezone.h>
#include <modules/space/rendering/renderablerings.h>
#include <modules/space/rendering/renderablesatellites.h>
#include <modules/space/rendering/renderablesmallbody.h>
#include <modules/space/rendering/renderablestars.h>
#include <modules/space/rendering/renderabletravelspeed.h>
#include <modules/space/rendering/simplespheregeometry.h>
#include <modules/space/translation/keplertranslation.h>
#include <modules/space/translation/spicetranslation.h>
#include <modules/space/translation/tletranslation.h>
#include <modules/space/translation/horizonstranslation.h>
#include <modules/space/rotation/spicerotation.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include "spacemodule_lua.inl"

namespace {
    constexpr openspace::properties::Property::PropertyInfo SpiceExceptionInfo = {
        "ShowExceptions",
        "Show Exceptions",
        "If enabled, errors from SPICE will be thrown and show up in the log. If "
        "disabled, the errors will be ignored silently."
    };
} // namespace

namespace openspace {

ghoul::opengl::ProgramObjectManager SpaceModule::ProgramObjectManager;

SpaceModule::SpaceModule()
    : OpenSpaceModule(Name)
    , _showSpiceExceptions(SpiceExceptionInfo, true)
{
    _showSpiceExceptions.onChange([&t = _showSpiceExceptions](){
        SpiceManager::ref().setExceptionHandling(SpiceManager::UseException(t));
    });
    addProperty(_showSpiceExceptions);
}

void SpaceModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<planetgeometry::PlanetGeometry>>(),
        "PlanetGeometry"
    );

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableConstellationBounds>(
        "RenderableConstellationBounds"
    );
    fRenderable->registerClass<RenderableFluxNodes>("RenderableFluxNodes");
    fRenderable->registerClass<RenderableHabitableZone>("RenderableHabitableZone");
    fRenderable->registerClass<RenderableRings>("RenderableRings");
    fRenderable->registerClass<RenderableSatellites>("RenderableSatellites");
    fRenderable->registerClass<RenderableSmallBody>("RenderableSmallBody");
    fRenderable->registerClass<RenderableStars>("RenderableStars");
    fRenderable->registerClass<RenderableTravelSpeed>("RenderableTravelSpeed");

    auto fTranslation = FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<KeplerTranslation>("KeplerTranslation");
    fTranslation->registerClass<SpiceTranslation>("SpiceTranslation");
    fTranslation->registerClass<TLETranslation>("TLETranslation");
    fTranslation->registerClass<HorizonsTranslation>("HorizonsTranslation");

    /*auto fTasks = FactoryManager::ref().factory<Task>();
    ghoul_assert(fTasks, "No task factory existed");
    fTasks->registerClass<volume::GenerateDebrisVolumeTask>("GenerateDebrisVolumeTask");*/

    auto fRotation = FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<SpiceRotation>("SpiceRotation");

    auto fGeometry = FactoryManager::ref().factory<planetgeometry::PlanetGeometry>();
    ghoul_assert(fGeometry, "Planet geometry factory was not created");
    fGeometry->registerClass<planetgeometry::SimpleSphereGeometry>("SimpleSphere");

    if (dictionary.hasValue<bool>(SpiceExceptionInfo.identifier)) {
        _showSpiceExceptions = dictionary.value<bool>(SpiceExceptionInfo.identifier);
    }
}

void SpaceModule::internalDeinitializeGL() {
    ProgramObjectManager.releaseAll(ghoul::opengl::ProgramObjectManager::Warnings::Yes);
}

std::vector<documentation::Documentation> SpaceModule::documentations() const {
    return {
        RenderableConstellationBounds::Documentation(),
        RenderableFluxNodes::Documentation(),
        RenderableHabitableZone::Documentation(),
        RenderableRings::Documentation(),
        RenderableSatellites::Documentation(),
        RenderableSmallBody::Documentation(),
        RenderableStars::Documentation(),
        RenderableTravelSpeed::Documentation(),
        SpiceRotation::Documentation(),
        SpiceTranslation::Documentation(),
        KeplerTranslation::Documentation(),
        TLETranslation::Documentation(),
        HorizonsTranslation::Documentation(),
        planetgeometry::PlanetGeometry::Documentation(),
        planetgeometry::SimpleSphereGeometry::Documentation()
    };
}

scripting::LuaLibrary SpaceModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "space";
    res.functions = {
        {
            "convertFromRaDec",
            &space::luascriptfunctions::convertFromRaDec,
            "string/double, string/double, double",
            "Returns the cartesian world position of a ra dec coordinate with distance. "
            "If the coordinate is given as strings the format should be ra 'XhYmZs' and "
            "dec 'XdYmZs'. If the coordinate is given as numbers the values should be "
            "in degrees."
        },
        {
            "convertToRaDec",
            &space::luascriptfunctions::convertToRaDec,
            "double, double, double",
            "Returns the formatted ra, dec strings and distance for a given cartesian "
            "world coordinate."
        }
    };

    return res;
}

} // namespace openspace
