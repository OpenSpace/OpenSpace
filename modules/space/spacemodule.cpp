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

#include <modules/space/spacemodule.h>

#include <modules/space/rendering/renderableconstellationbounds.h>
#include <modules/space/rendering/renderableconstellationlines.h>
#include <modules/space/rendering/renderableeclipsecone.h>
#include <modules/space/rendering/renderablefluxnodes.h>
#include <modules/space/rendering/renderablehabitablezone.h>
#include <modules/space/rendering/renderableorbitalkepler.h>
#include <modules/space/rendering/renderablerings.h>
#include <modules/space/rendering/renderablestars.h>
#include <modules/space/rendering/renderabletravelspeed.h>
#include <modules/space/translation/keplertranslation.h>
#include <modules/space/translation/spicetranslation.h>
#include <modules/space/translation/gptranslation.h>
#include <modules/space/translation/horizonstranslation.h>
#include <modules/space/rotation/spicerotation.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/coordinateconversion.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include "spacemodule_lua.inl"

namespace {
    constexpr openspace::properties::Property::PropertyInfo SpiceExceptionInfo = {
        "ShowExceptions",
        "Show Exceptions",
        "If enabled, errors from SPICE will be thrown and show up in the log. If "
        "disabled, the errors will be ignored silently.",
        openspace::properties::Property::Visibility::Developer
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
    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableConstellationBounds>(
        "RenderableConstellationBounds"
    );
    fRenderable->registerClass<RenderableConstellationLines>(
        "RenderableConstellationLines"
    );
    fRenderable->registerClass<RenderableEclipseCone>("RenderableEclipseCone");
    fRenderable->registerClass<RenderableFluxNodes>("RenderableFluxNodes");
    fRenderable->registerClass<RenderableHabitableZone>("RenderableHabitableZone");
    fRenderable->registerClass<RenderableRings>("RenderableRings");
    fRenderable->registerClass<RenderableOrbitalKepler>("RenderableOrbitalKepler");
    fRenderable->registerClass<RenderableStars>("RenderableStars");
    fRenderable->registerClass<RenderableTravelSpeed>("RenderableTravelSpeed");

    ghoul::TemplateFactory<Translation>* fTranslation =
        FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<KeplerTranslation>("KeplerTranslation");
    fTranslation->registerClass<SpiceTranslation>("SpiceTranslation");
    fTranslation->registerClass<GPTranslation>("GPTranslation");
    fTranslation->registerClass<HorizonsTranslation>("HorizonsTranslation");

    ghoul::TemplateFactory<Rotation>* fRotation =
        FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<SpiceRotation>("SpiceRotation");

    if (dictionary.hasValue<bool>(SpiceExceptionInfo.identifier)) {
        _showSpiceExceptions = dictionary.value<bool>(SpiceExceptionInfo.identifier);
    }
}

void SpaceModule::internalDeinitializeGL() {
    ProgramObjectManager.releaseAll(ghoul::opengl::ProgramObjectManager::Warnings::Yes);
}

std::vector<documentation::Documentation> SpaceModule::documentations() const {
    return {
        HorizonsTranslation::Documentation(),
        KeplerTranslation::Documentation(),
        RenderableConstellationBounds::Documentation(),
        RenderableConstellationLines::Documentation(),
        RenderableEclipseCone::Documentation(),
        RenderableFluxNodes::Documentation(),
        RenderableHabitableZone::Documentation(),
        RenderableRings::Documentation(),
        RenderableOrbitalKepler::Documentation(),
        RenderableStars::Documentation(),
        RenderableTravelSpeed::Documentation(),
        SpiceRotation::Documentation(),
        SpiceTranslation::Documentation(),
        GPTranslation::Documentation()
    };
}

scripting::LuaLibrary SpaceModule::luaLibrary() const {
    return {
        .name = "space",
        .functions = {
            codegen::lua::ConvertFromRaDec,
            codegen::lua::ConvertToRaDec,
            codegen::lua::ReadKeplerFile
        },
        .scripts = {
            absPath("${MODULE_SPACE}/scripts/spice.lua")
        }
    };
}

} // namespace openspace
