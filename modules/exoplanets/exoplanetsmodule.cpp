/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/openspaceengine.h>

//#include <openspace/rendering/renderable.h>
//#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

namespace openspace {

ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

int addNode(lua_State* L) {

    // get name of star and add a node at the position of the star.
    // position is found in the star-files
    // after the node has been added one should be able to select it as a focus point.

    //start

    const int StringLocation = -1; //first argument
    const std::string starname = luaL_checkstring(L, StringLocation);

    //printf(starname.c_str());

    // adding the parent node of the exoplanet system
    const std::string luaTableParent = "{ Name = '" + starname +"', Parent = 'SolarSystemBarycenter', Transform = { Translation = { Type = 'StaticTranslation', Position = {4662120063743.592773, 1263245003503.724854, -955413856565.788086} } }}"; // positionen must be gathered from star.speck
    const std::string scriptParent = "openspace.addSceneGraphNode("+ luaTableParent +");";
    OsEng.scriptEngine().queueScript(
       scriptParent,
       openspace::scripting::ScriptEngine::RemoteScripting::Yes
    ); 

    // adding a renderable in the place of the stars
    const std::string luaTableStarGlare = "{ Name = '" + starname + "Plane', Parent = '" + starname +"', Renderable = { Type = 'RenderablePlaneImageLocal', Size = 1.3*10^10.5, Billboard = true, Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/glare.png', BlendMode = 'Additive' }  }";
    const std::string scriptGlare= "openspace.addSceneGraphNode("+ luaTableStarGlare +");";
    OsEng.scriptEngine().queueScript(
       scriptGlare,
       openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );


}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {

    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addNode",
            &addNode,
            {},
            "string",
            "Adds print message."
        }

    };

    return res;
}

//void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {
    //auto fRenderable = FactoryManager::ref().factory<Renderable>();
    //ghoul_assert(fRenderable, "No renderable factory existed");

    //fRenderable->registerClass<RenderableDebugPlane>("RenderableDebugPlane");
//}

//std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    //return {
        //RenderableDebugPlane::Documentation()
    //};
//}


} // namespace openspace
